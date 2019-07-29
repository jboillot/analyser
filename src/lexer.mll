{
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


  open Parser
  open Lexing

  exception Error of string
}

let whitespace_char_no_newline = [' ' '\t' '\r']

rule initial = parse
| whitespace_char_no_newline+ { initial lexbuf }
| '\n' { new_line lexbuf; initial lexbuf }
| "/*" { multiline_comment lexbuf; initial lexbuf }
| "//" { singleline_comment lexbuf; initial lexbuf }
| ';' { SEMICOLON }
| ['0'-'9']+ as i { INT(Z.of_string i) }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIV }
| '=' { EQUAL }
| '~' { BITNOT }
| '&' { ADDR }
| "int" { C_INT }
| "true" { TRUE }
| "false" { FALSE }
| "if" { IF }
| "else" { ELSE }
| "while" { WHILE }
| "for" { FOR }
| "return" { RETURN }
| "&&" { AND }
| "||" { OR }
| "==" { EQ }
| "!=" { NEQ }
| "<" { LT }
| "<=" { LE }
| ">" { GT }
| ">=" { GE }
| "!" { NOT }
| "?" { QUESTION }
| "asm" { ASM }
| "eax" { EAX_REG }
| "ebx" { EBX_REG }
| "ecx" { ECX_REG }
| "edx" { EDX_REG }
| "MOV" { MOV }
| "JMP" { JMP }
| "CALL" { CALL }
| "RET" { RET }
| "PUSH" { PUSH }
| "POP" { POP }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACKET }
| '}' { RBRACKET }
| '[' { LSTR8BRACKET }
| ']' { RSTR8BRACKET }
| ',' { COMMA }
| ':' { COLON }
| '@' { AT }
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']* as s { ID(s) }
| eof { EOF }
| _ { raise (Error("Unexpected token!")) }

and multiline_comment = parse
| "*/" { () }
| eof { raise (Error("Unterminated comment!")) }
| '\n' { new_line lexbuf; multiline_comment lexbuf }
| _ { multiline_comment lexbuf }

and singleline_comment = parse
| '\n' { new_line lexbuf }
| eof { () }
| _ { singleline_comment lexbuf }

and ruleTail acc = parse
| eof { acc }
| '\n' { acc }
| [^'\n']* as str { ruleTail (acc ^ str) lexbuf }