%{
  open Ast
%}

%token <string> ID
%token SELECT FROM COMMA SEMICOLON AS
%start stmt
%type <Ast.stmt option> stmt

%%

stmt:
  | SELECT fields FROM ID SEMICOLON { Some(Select($2, $4, None)) }
  | SELECT fields FROM ID AS ID SEMICOLON { Some(Select($2, $4, Some($6))) }
  | error { None }
;

fields:
  | ID { [Identifier $1] }
  | ID COMMA fields { Identifier $1 :: $3 }
;

%%
