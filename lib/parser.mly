%{ open Ast %}

%token <string> IDENT
%token ADD SUB MUL EQ
%token LAMBDA ARROW
%token LPAR RPAR
%token LBRA RBRA
%token NIL CONS TL HD COMMA
%token IFZ IFE THEN ELSE
%token REC
%token LET IN
%token UNIT REF DEREF ASSIGN
%token EOF
%token <int> NUM

%type <Ast.pterm> prog
%type <Ast.pterm> expr

%start prog

%%
prog :
  | expr EOF { $1 }
;

expr :
  | IDENT { Var $1 }
  | LPAR expr expr RPAR { App ($2, $3) }
  | LAMBDA ident_list ARROW expr {
      let rec make_abs args body = match args with
        | [] -> body
        | id::rest -> Abs(id, make_abs rest body)
      in make_abs $2 $4
    }

  | NUM { Int $1 }
  | expr ADD expr { Add ($1, $3) }
  | expr SUB expr { Sub ($1, $3) }
  | expr MUL expr { Mul ($1, $3) }

  | LBRA expr_list RBRA { $2 }
  | expr CONS expr { Cons ($1, $3) }
  | NIL { Nil }
  | HD expr { Hd $2 }
  | TL expr { Tl $2 }

  | IFZ expr THEN expr ELSE expr { IfZero ($2, $4, $6) }
  | IFE expr THEN expr ELSE expr { IfEmpty ($2, $4, $6) }
  | LET REC ident_list EQ expr IN expr {
      let rec make_abs = function
        | [] -> $5
        | id::rest -> Abs(id, make_abs rest)
      in Let (List.hd $3, Fix (Abs (List.hd $3, make_abs (List.tl $3))), $7)
    }
  | LET IDENT EQ expr IN expr { Let ($2, $4, $6) }

  | UNIT { Unit }
  | REF expr { Ref $2 }
  | DEREF expr { Deref $2 }
  | expr ASSIGN expr { Assign ($1, $3) }
  | LPAR expr RPAR { $2 }
;

ident_list :
  | IDENT { [$1] }
  | IDENT ident_list { $1 :: $2 }
;

expr_list :
  | expr { Cons($1, Nil) }
  | expr COMMA expr_list { Cons($1, $3) }
;;
