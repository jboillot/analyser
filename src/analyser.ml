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

open Debug
open Analysis
open Utils
open Options

let main () =
  let speclist = [("-v", Arg.Set verbose, "Enables verbose mode");
                  ("-withoutLocs", Arg.Set withoutLocs, "Remove locations from output program");
                  ("-withoutWidening1", Arg.Set withoutWidening1, "Disable widening termination. Enlarge abstract domains to find a lfp!");
                  ("-withoutWidening2", Arg.Set withoutWidening2, "Disable widening termination. Exclude cases to find a lfp and then join all cases!")] in
  let usage_msg = "My little program of internship. Options available: " in
  Arg.parse speclist print_endline usage_msg;
  let argc = Array.length Sys.argv in
  let filename = Sys.argv.(argc - 1) in
  let ic = if argc > 1 && Sys.file_exists filename
    then open_in filename
    else
      begin
        print_endline "Analyser Copyright (C) 2019 Jérôme Boillot";
        print_endline "This program comes with ABSOLUTELY NO WARRANTY.";
        print_endline "This is free software, and you are welcome to redistribute it.";
        stdin
      end
  in
  let lexbuf = Lexing.from_channel ic in
  begin try
      let unlocated_program = Parser.main Lexer.initial lexbuf in
      let program = addLocationsProgram unlocated_program in
      let program = removeLabelsProgram program in
      print_endline "### Program:";
      print_string (program2string program);
      print_newline ();
      analyseProgram program
    with
    | Lexer.Error(reason) ->
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      let tail = Lexer.ruleTail "" lexbuf in
      Printf.eprintf "Lexer error detected on position %d:%d. The token read is %s and the end of the line is %s%! The reason provided is %s\n" line cnum tok tail reason
    | Parser.Error ->
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      let tail = Lexer.ruleTail "" lexbuf in
      Printf.eprintf "Parsing error detected on position %d:%d. The token read is %s and the end of the line is %s%!\n" line cnum tok tail
  end;
  close_in ic

let _ = main ()
