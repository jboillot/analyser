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

(** Module used to manipulate abstract domain of pointers as a sorted list *)

(** Pointer induction to a variable name *)
type ptr_var = PtrVar of ptr_var | LastPtrVar of string

(** Sorted list of pointer variables *)
type sortedList = ptr_var list

(** Function of comparison of pointer variables
    @param a The first pointer variable
    @param b the second pointer variable
    @return The comparison result of the two pointer variables *)
let rec cmp_ptr_var a b : int =
  match (a, b) with
  | (LastPtrVar(a'), LastPtrVar(b')) -> compare a' b'
  | (PtrVar(_), LastPtrVar(_)) -> 1
  | (LastPtrVar(_), PtrVar(_)) -> -1
  | (PtrVar(a'), PtrVar(b')) -> cmp_ptr_var a' b'

(** Add an element to a sorted list. Complexity in the worst case in linear time. Avoids duplication.
    @param element The new pointer variable
    @param l The last abstract domain of pointer variables
    @return The new abstract domain of pointer variables *)
let rec addElement element l : sortedList =
  match l with
  | [] -> [element]
  | h::t when cmp_ptr_var h element < 0 -> h::(addElement element t)
  | h::t when cmp_ptr_var h  element = 0 -> h::t
  | h::t -> element::h::t

(** Merge tow sorted lists. Complexity in the worst case in linear time. Avoids duplication.
    @param l1 The first abstract domain of pointer variables
    @param l2 The second abstract domain of pointer variables
    @return The new abstract domain of pointer variables which is the merge of the two given as parameter *)
let rec mergeSortedList l1 l2 : sortedList =
  match (l1, l2) with
  | ([], other) | (other, []) -> other
  | (h1::t1, h2::_) when cmp_ptr_var h1 h2 < 0 -> h1::(mergeSortedList t1 l2)
  | (h1::_, h2::t2) when cmp_ptr_var h1 h2 = 0 -> mergeSortedList l1 t2
  | (_, h2::t2) -> h2::(mergeSortedList l1 t2)

(** Return the meet of tow sorted lists. Complexity in the worst case in linear time.
    @param l1 The first abstract domain of pointer variables
    @param l2 The second abstract domain of pointer variables
    @return The new abstract domain of pointer variables which is the meet of the two given as parameter *)
let rec meetSortedList l1 l2 : sortedList =
  match (l1, l2) with
  | ([], other) | (other, []) -> other
  | (h1::t1, h2::_) when cmp_ptr_var h1 h2 < 0 -> meetSortedList t1 l2
  | (h1::_, h2::t2) when cmp_ptr_var h1 h2 > 0 -> meetSortedList l1 t2
  | (h1::t1, _::t2) -> h1::(meetSortedList t1 t2)

(** Return true iff the element is in the sorted list. In linear time in the worst case.
    @param element The pointer variable to find
    @param l The abstract domain of pointer variables in which we search
    @return Is [element] in [l] *)
let rec isInSortedList element l : bool =
  match l with
  | [] -> false
  | h::t when cmp_ptr_var h element < 0 -> isInSortedList element t
  | h::_ when cmp_ptr_var h element = 0 -> true
  | _ -> false