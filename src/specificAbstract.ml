(*
Analyser is a static analyser which finds errors and undefinded behaviors in C programs with inline assembly.

Copyright 2019 Jérôme Boillot

Permission is hereby granted, free of charge, to any person obtaining a copy of this
software and associated documentation files (the "Software"), to deal in the Software
without restriction, including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.
*)

open Types
open Utils
open Exceptions


let lastStat = ref (StatC(Loc_c(-1), Declare(Int_c, "42 - This statement is just the first one, don't worry!"), Loc_c(0)))

let alpha real_elem : abstrD =
  match real_elem with
  | RealD([]) | RealBot -> AbstrBot
  | RealD(l) -> AbstrD(minmax l)

let join a1 a2 : abstrD =
  match (a1, a2) with
  | (AbstrBot, other) | (other, AbstrBot) -> other
  | (AbstrPtr(l1), AbstrPtr(l2)) -> AbstrPtr(IdSet.union l1 l2)
  | (AbstrPtr(_), _) | (_, AbstrPtr(_)) -> raiseException PTRINTMATCH !lastStat; AbstrBot
  | (AbstrD(n11, n12), AbstrD(n21, n22)) -> AbstrD(min n11 n21, max n12 n22)
  | (AbstrLoc(l1), AbstrLoc(l2)) when l1 = l2 -> a1
  | (AbstrLoc(_), _) | (_, AbstrLoc(_)) -> raiseException LOCMATCH !lastStat; AbstrBot

let meet a1 a2 : abstrD =
  (*print_endline ("Meet " ^ abstrD2string a1 ^ " and " ^ abstrD2string a2);*)
  match (a1, a2) with
  | (AbstrBot, _) | (_, AbstrBot) -> AbstrBot
  | (AbstrPtr(l1), AbstrPtr(l2)) -> AbstrPtr(IdSet.inter l1 l2)
  | (AbstrPtr(_), _) | (_, AbstrPtr(_)) -> raiseException PTRINTMATCH !lastStat; AbstrBot
  | (AbstrLoc(l1), AbstrLoc(l2)) when l1 = l2 -> a1
  | (AbstrLoc(_), _) | (_, AbstrLoc(_)) -> raiseException PTRINTMATCH !lastStat; AbstrBot
  | (AbstrD(n11, n12), AbstrD(n21, n22)) ->
    (*print_endline (Z.to_string n11 ^ " " ^ Z.to_string n12 ^ " " ^ Z.to_string n21 ^ " " ^ Z.to_string n22);*)
    begin if n11 < abstrMin || n12 > abstrMax || n21 < abstrMin || n22 > abstrMax
      then raiseException OVERFLOW !lastStat
    end;
    let actualMin = max n11 n21 in
    let actualMax = min n12 n22 in
    if actualMin > actualMax
    then AbstrBot
    else AbstrD(actualMin, actualMax)

let isInAbstractDomain n p : bool =
  (*print_endline ("Compare " ^ Z.to_string n ^ " and " ^ abstrD2string p);*)
  match p with
  | AbstrBot | AbstrPtr(_) | AbstrLoc(_) -> false
  | AbstrD(n1, n2) -> n1 <= n && n <= n2

let faexpUna op a : abstrD =
  let toMeet =
    match a with
    | AbstrBot -> AbstrBot
    | AbstrPtr(_) | AbstrLoc(_) -> raiseException PTRINTMATCH !lastStat; AbstrBot
    | AbstrD(n1, n2) ->
      match op with
      | MinusUna ->  AbstrD(Z.neg n2, Z.neg n1)
      | BitwiseNot -> AbstrD(Z.lognot n2, Z.lognot n1)
  in meet toMeet abstrTop

let faexpBin a1 op a2 : abstrD =
  match (a1, a2) with
  | (AbstrBot,_) | (_, AbstrBot) -> AbstrBot
  | (AbstrPtr(_), _) | (_, AbstrPtr(_)) -> raiseException PTRINTMATCH !lastStat; AbstrBot
  | (AbstrLoc(_), _) | (_, AbstrLoc(_)) -> raiseException PTRINTMATCH !lastStat; AbstrBot
  | (AbstrD(n11, n12), AbstrD(n21, n22)) ->
    match op with
    | Plus -> meet (AbstrD(Z.add n11 n21, Z.add n12 n22)) abstrTop
    | Minus -> meet (AbstrD(Z.sub n11 n22, Z.sub n12 n21)) abstrTop
    | Times -> meet
                 (AbstrD(min (min (Z.mul n11 n21) (Z.mul n11 n22)) (min (Z.mul n12 n21) (Z.mul n12 n22)),
                         max (max (Z.mul n11 n21) (Z.mul n11 n22)) (max (Z.mul n12 n21) (Z.mul n12 n22))))
                 abstrTop
    | Div ->
      begin
        if isInAbstractDomain (Z.of_int 0) a2
        then raiseException DIVIDE_BY_ZERO !lastStat
      end;
      if n21 = Z.zero && n22 = Z.zero
      then AbstrBot
      else
        begin
          if n21 < Z.zero && n22 > Z.zero then
            meet (AbstrD(min n11 (Z.neg n12), max n12 (Z.neg n11))) abstrTop
          else
            begin
              let (n21, n22) =
                if n21 = Z.zero then (Z.one, n22)
                else if n22 = Z.zero then (n21, Z.minus_one)
                else (n21, n22)
              in
              let p11 = Z.div n11 n21 in
              let p12 = Z.div n11 n22 in
              let p21 = Z.div n21 n21 in
              let p22 = Z.div n12 n22 in
              meet (AbstrD(min (min p11 p12) (min p21 p22),
                           max (max p11 p12) (max p21 p22))) abstrTop
            end

        end

let rec abexpArithBin a1 op a2 : abstrD * abstrD =
  match (a1, a2) with
  | (AbstrBot, _) | (_, AbstrBot) -> (AbstrBot, AbstrBot)
  | (AbstrPtr(l1), AbstrPtr(l2)) ->
    begin match op with
      | Eq -> let l3 = IdSet.inter l1 l2 in if IdSet.is_empty l3 then (AbstrBot, AbstrBot) else (AbstrPtr(l3), AbstrPtr(l3))
      | Neq ->
        if IdSet.cardinal l1 = 1 && IdSet.equal l1 l2
        then (AbstrBot, AbstrBot)
        else (a1, a2)
      | _ -> raiseException PTRINTMATCH !lastStat; (AbstrBot, AbstrBot)
    end
  | (AbstrPtr(_), _) | (_, AbstrPtr(_)) -> raiseException PTRINTMATCH !lastStat; (AbstrBot, AbstrBot)
  | (AbstrLoc(l1), AbstrLoc(l2)) ->
    begin match op with
      | Eq -> if l1 = l2 then (a1, a2) else (AbstrBot, AbstrBot)
      | Neq -> if l1 = l2 then (AbstrBot, AbstrBot) else (a1, a2)
      | _ -> raiseException PTRINTMATCH !lastStat; (AbstrBot, AbstrBot)
    end
  | (AbstrLoc(_), _) | (_, AbstrLoc(_)) -> raiseException PTRINTMATCH !lastStat; (AbstrBot, AbstrBot)
  | (AbstrD(n11, n12), AbstrD(n21, n22)) ->
    match op with
    | Eq -> let a3 = meet a1 a2 in (a3, a3)
    | Neq ->
      begin match (a1, a2) with
        | _ when n11 = n12 && n21 = n22 && n11 = n21 -> (AbstrBot, AbstrBot)
        | _ when n11 = n12 && n11 = n21 -> (AbstrD(n11, n12), AbstrD(Z.add n21 Z.one, n22))
        | _ when n11 = n12 && n11 = n22 -> (AbstrD(n11, n12), AbstrD(n21, Z.sub n22 Z.one))
        | _ when n21 = n22 && n11 = n21 -> (AbstrD(Z.add n11 Z.one, n12), AbstrD(n21, n22))
        | _ when n21 = n22 && n12 = n21 -> (AbstrD(n11, Z.sub n12 Z.one), AbstrD(n21, n22))
        | _ -> (a1, a2)
      end
    | Lt -> (meet (AbstrD(n11, min n12 (Z.sub n22 Z.one))) a1), (meet (AbstrD(max (Z.add n11 Z.one) n21, n22)) a2)
    | Le -> (meet (AbstrD(n11, min n12 n22)) a1), (meet (AbstrD(max n11 n21, n22)) a2)
    | Gt -> let (p2, p1) = abexpArithBin a2 Lt a1 in (p1, p2)
    | Ge -> let (p2, p1) = abexpArithBin a2 Le a1 in (p1, p2)

let baexpUna op a p : abstrD =
  meet (faexpUna op p) a (* Because we have involutory functions *)

let baexpBin a1 op a2 p : abstrD * abstrD =
  (*print_endline ("a1=" ^ abstrD2string a1 ^ ", a2=" ^ abstrD2string a2 ^ ", p=" ^ abstrD2string p);*)
  match (a1, a2, p) with
  | (AbstrBot,_,_) | (_,AbstrBot,_) | (_,_,AbstrBot) -> (AbstrBot, AbstrBot)
  | (AbstrPtr(_),_,_) | (_,AbstrPtr(_),_) | (_,_,AbstrPtr(_)) -> raiseException PTRINTMATCH !lastStat; (AbstrBot, AbstrBot)
  | (AbstrLoc(_),_,_) | (_,AbstrLoc(_),_) | (_,_,AbstrLoc(_)) -> raiseException PTRINTMATCH !lastStat; (AbstrBot, AbstrBot)
  | (AbstrD(a11, a12), AbstrD(a21, a22), AbstrD(p1, p2)) ->
    match op with
    | Plus -> (meet (AbstrD(Z.sub p1 a22, Z.sub p2 a21)) a1, meet (AbstrD(Z.sub p1 a12, Z.sub p2 a11)) a2)
    | Minus -> (meet (AbstrD(Z.add a21 p1, Z.add p2 a22)) a1, meet (AbstrD(Z.sub a11 p2, Z.sub a12 p1)) a2)
    | Times ->
      let zeroInP = isInAbstractDomain Z.zero p in
      let result1 =
        if zeroInP && isInAbstractDomain Z.zero a2 then
          a1
        else
          begin
            let p11 = if a21 = Z.zero then min (if zeroInP then Z.zero else a11) (Z.div p1 Z.one) else Z.div p1 a21 in
            let p12 = if a22 = Z.zero then min (if zeroInP then Z.zero else a11) (Z.div p1 Z.minus_one) else Z.div p1 a22 in
            let p21 = if a21 = Z.zero then max (if zeroInP then Z.zero else a12) (Z.div p2 Z.one) else Z.div p2 a21 in
            let p22 = if a22 = Z.zero then max (if zeroInP then Z.zero else a12) (Z.div p2 Z.minus_one) else Z.div p2 a22 in
            meet (AbstrD(min p11 p12, max p21 p22)) a1
          end
      in
      let result2 =
        if zeroInP && isInAbstractDomain Z.zero a1 then
          a2
        else
          begin
            let p11 = if a11 = Z.zero then min (if zeroInP then Z.zero else a21) (Z.div p1 Z.one) else Z.div p1 a11 in
            let p12 = if a12 = Z.zero then min (if zeroInP then Z.zero else a21) (Z.div p1 Z.minus_one) else Z.div p1 a12 in
            let p21 = if a11 = Z.zero then max (if zeroInP then Z.zero else a22) (Z.div p2 Z.one) else Z.div p2 a11 in
            let p22 = if a12 = Z.zero then max (if zeroInP then Z.zero else a22) (Z.div p2 Z.minus_one) else Z.div p2 a12 in
            meet (AbstrD(min p11 p12, max p21 p22)) a2
          end
      in (result1, result2)
    | Div ->
      begin
        if isInAbstractDomain Z.zero a2
        then raiseException DIVIDE_BY_ZERO !lastStat
      end;
      if a21 = Z.zero && a22 = Z.zero
      then (AbstrBot, AbstrBot)
      else
        begin
          failwith "baexpBin: The division in boolean expressions is not supported yet!"
          (*let (a21, a22) =
            if a21 = Z.zero then (Z.one, a22)
            else if a22 = Z.zero then (a21, Z.minus_one)
            else (a21, a22)
            in
            (*let max11 = Z.add (Z.mul p1 a21) (Z.sub a21 Z.one) in
            let max11 = if max11 > abstrMax && Z.mul p1 a21 <= abstrMax then abstrMax else max11 in
            let max12 = Z.add (Z.mul p1 a22) (Z.sub a22 Z.one) in
            let max12 = if max12 > abstrMax && Z.mul p1 a22 <= abstrMax then abstrMax else max12 in
            let max13 = Z.add (Z.mul p2 a21) (Z.sub a21 Z.one) in
            let max13 = if max13 > abstrMax && Z.mul p2 a21 <= abstrMax then abstrMax else max13 in
            let max14 = Z.add (Z.mul p2 a22) (Z.sub a22 Z.one) in
            let max14 = if max14 > abstrMax && Z.mul p2 a22 <= abstrMax then abstrMax else max14 in*)

            let realmin1 = min (min (Z.mul p1 a21) (Z.mul p1 a22)) (min (Z.mul p2 a21) (Z.mul p2 a22)) in
            let realmin1 =
            if p1 = Z.zero || p2 = Z.zero
            then min (Z.add (Z.neg (Z.abs a22)) Z.one) realmin1
            else realmin1
            in
            (*let realmax1 = max (max max11 max12) (max max13 max14) in
            let realmax1 =
            if p1 = Z.zero || p2 = Z.zero
            then max (Z.sub (Z.abs a21) Z.one) realmax1
            else realmax1
            in*)
            let realmax1 = a12 in

            (meet (AbstrD(realmin1, realmax1)) a1,
            meet (if isInAbstractDomain Z.zero p then
                   begin
                     if isInAbstractDomain Z.zero a1 then abstrTop
                     else if realmin1 = abstrMin && realmax1 = abstrMax then AbstrBot
                     else abstrTop (* It is possible to be more precise when realmin1 = abstrMin or realmax1 = abstrMax but it's complicated. *)
                   end
                   (* Absolutely not sure that this part works! *)
                 else AbstrD(invMin (invMin (Z.div a11 p1) (Z.div a12 p1)) (invMin (Z.div a11 p2) (Z.div a12 p2))
                            , invMax (invMax (Z.div a11 p1) (Z.div a12 p1)) (invMax (Z.div a11 p2) (Z.div a12 p2))))
             a2)*)
        end

let widening a1 a2 : abstrD =
  match (a1, a2) with
  | (AbstrBot, _) | (_, AbstrBot) -> AbstrBot
  | (AbstrPtr(l1), AbstrPtr(l2)) when l1 = l2 -> AbstrPtr(l1)
  | (AbstrPtr(_), _) | (_, AbstrPtr(_)) -> raiseException PTRINTMATCH !lastStat; AbstrBot
  | (AbstrLoc(l1), AbstrLoc(l2)) when l1 = l2 -> AbstrLoc(l1)
  | (AbstrLoc(_), _) | (_, AbstrLoc(_)) -> raiseException LOCMATCH !lastStat; AbstrBot
  | (AbstrD(a11, a12), AbstrD(a21, a22)) ->
    AbstrD(begin if a11 <= a21 then a11 else abstrMin end, begin if a12 >= a22 then a12 else abstrMax end)