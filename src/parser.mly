%{
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
open SortedList
%}

%token <Z.t> INT
%token <string> ID
%token PLUS MINUS TIMES DIV
%token EQUAL
%token C_INT
%token IF ELSE WHILE
%token TRUE FALSE
%token RETURN
%token AND OR LT LE GT GE EQ NEQ NOT BITNOT ADDR
%token ASM JMP MOV CALL RET PUSH POP
%token EAX_REG EBX_REG ECX_REG EDX_REG
%token LPAREN RPAREN LBRACKET RBRACKET LSTR8BRACKET RSTR8BRACKET
%token SEMICOLON COMMA COLON AT
%token EOF

/* lowest precedence */
%left AND OR
%left PLUS MINUS
%left TIMES DIV
%nonassoc NOT
/* highest precedence */

%start main
%type <Types.global_statement list> main

%%
main:
| EOF { [] }
| c_function main { $1::$2 }
;

c_function:
| c_type ID LPAREN arguments_typed RPAREN LBRACKET c_statement_list RBRACKET { FunctionWithoutLoc($1, $2, $4, $7) }
;

arguments_typed:
| c_type ID COMMA arguments_typed { ($1, $2)::$4 }
| c_type ID { [($1, $2)] }
| { [] }
;

arguments:
| arith_expr COMMA arguments { $1::$3 }
| arith_expr { [($1)] }
| { [] }
;

c_statement:
| c_type ID SEMICOLON { Declare($1, $2) }
| c_type ID EQUAL arith_expr SEMICOLON { DeclareAssign($1, $2, $4) }
| ID EQUAL arith_expr SEMICOLON { Assign($1, $3) }
| ID EQUAL ID LPAREN arguments RPAREN SEMICOLON { CallAssignC($1, $3, $5) }

| ID PLUS EQUAL arith_expr SEMICOLON { Assign($1, ArithArithBinExpr(Var($1), Plus, $4)) }
| ID MINUS EQUAL arith_expr SEMICOLON { Assign($1, ArithArithBinExpr(Var($1), Minus, $4)) }
| ID MINUS TIMES arith_expr SEMICOLON { Assign($1, ArithArithBinExpr(Var($1), Times, $4)) }
| ID MINUS DIV arith_expr SEMICOLON { Assign($1, ArithArithBinExpr(Var($1), Div, $4)) }

/* I expect a shift/reduce conflict here. It is inevitable but the shift strategy (which is good in this context) will be applied. */
| IF LPAREN bool_expr RPAREN c_block ELSE c_block { BranchWithoutLoc($3, $5, $7) }
| IF LPAREN bool_expr RPAREN c_block { BranchWithoutLoc($3, $5, []) }

| ASM LBRACKET asm_statement_list RBRACKET SEMICOLON { AsmWithoutLoc($3) }
| ptr_expr EQUAL arith_expr SEMICOLON { PtrAssign($1, $3) }

| WHILE LPAREN bool_expr RPAREN c_block { WhileWithoutLoc($3, $5) }

| RETURN arith_expr SEMICOLON { Return($2) }
;

c_block:
| c_statement {[$1]}
| LBRACKET c_statement_list RBRACKET { $2 }
;

c_statement_list:
| c_statement c_statement_list { $1::$2 }
| { [] }
;

asm_statement:
| ID COLON { Label($1) }
| MOV asm_mutable COMMA asm_value { Mov($2, $4) }
| JMP jmp { Jmp($2) }
| CALL jmp { Call($2) }
| RET { Ret }
| PUSH asm_value { Push($2) }
| POP asm_mutable { Pop($2) }
;

asm_statement_list:
| asm_statement asm_statement_list { $1::$2 }
| { [] }
;

arith_expr:
| simple_arith_expr { $1 }
| constant { Cons($1) }
| LSTR8BRACKET constant COMMA constant RSTR8BRACKET { Interval($2, $4) }
| MINUS simple_arith_expr { ArithArithUnaExpr(MinusUna, $2) }
| BITNOT simple_arith_expr { ArithArithUnaExpr(BitwiseNot, $2) }
| arith_expr PLUS arith_expr { ArithArithBinExpr($1, Plus, $3) }
| arith_expr MINUS arith_expr { ArithArithBinExpr($1, Minus, $3) }
| arith_expr TIMES arith_expr { ArithArithBinExpr($1, Times, $3) }
| arith_expr DIV arith_expr { ArithArithBinExpr($1, Div, $3) }
;

constant:
| INT { if $1 <= Types.abstrMax then $1 else failwith "Lexer: Number too big" }
| MINUS INT { let r = Z.neg $2 in if r >= Types.abstrMin then r else failwith "Lexer: Number too small" }
;

simple_arith_expr:
| LPAREN arith_expr RPAREN { $2 }
| ID { Var($1) }
| TIMES simple_arith_expr { Deref($2) }
| ADDR ID { Addr($2) }
;

bool_expr:
| TRUE { True }
| FALSE { False }
| LPAREN bool_expr RPAREN { $2 }
| bool_expr AND bool_expr { BoolBinExpr($1, And, $3) }
| bool_expr OR bool_expr { BoolBinExpr($1, Or, $3) }
| NOT bool_expr { BoolUnaExpr(Not, $2) }
| arith_expr EQ arith_expr { BoolArithBinExpr($1, Eq, $3) }
| arith_expr NEQ arith_expr { BoolArithBinExpr($1, Neq, $3) }
| arith_expr LT arith_expr { BoolArithBinExpr($1, Lt, $3) }
| arith_expr LE arith_expr { BoolArithBinExpr($1, Le, $3) }
| arith_expr GT arith_expr { BoolArithBinExpr($1, Gt, $3) }
| arith_expr GE arith_expr { BoolArithBinExpr($1, Ge, $3) }
;

ptr_expr:
| TIMES ID { LastPtrVar($2) }
| TIMES ptr_expr { PtrVar($2) }
;

c_type:
| C_INT { Int_c }
| c_type TIMES { Ptr($1) }
;

asm_mutable:
| reg_32 { AsmReg($1) }
| ID { AsmVar($1) }
;

asm_value:
| constant { AsmCst($1) }
| reg_32 { AsmReg($1) }
| ID { AsmVar($1) }
| AT ID { AsmLbl($2) }
| LSTR8BRACKET reg_32 RSTR8BRACKET { AsmPtr(AsmReg($2)) }
| LSTR8BRACKET ID RSTR8BRACKET { AsmPtr(AsmVar($2)) }
;

jmp:
| reg_32 { AsmReg($1) }
| ID { AsmVar($1) }
| AT ID { AsmLbl($2) }
;

reg_32:
| EAX_REG { EAX }
| EBX_REG { EBX }
| ECX_REG { ECX }
| EDX_REG { EDX }
;