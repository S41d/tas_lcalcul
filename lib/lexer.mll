{
  open Parser
  exception Eof
}

rule token = parse
  [' ' '\t' '\n' '\r']       { token lexbuf }     (* skip blanks *)
  (* Symboles reserves *)
  | '['              { LBRA }
  | ']'              { RBRA }
  | '('              { LPAR }
  | ')'              { RPAR }
  | '+'              { ADD }
  | '-'              { SUB }
  | '*'              { MUL }
  | '!'              { DEREF }
  | '='              { EQ }
  | "->"             { ARROW }
  | ":="             { ASSIGN }
  | ","              { COMMA }
  | "::"             { CONS }
  | "[]"             { NIL }
  | "\\"             { LAMBDA }
  (* Mots clef *)
  | "fun"            { LAMBDA }
  | "hd"             { HD }
  | "tl"             { TL }
  | "nil"            { NIL }
  | "ifz"            { IFZ }
  | "ife"            { IFE }
  | "then"           { THEN }
  | "else"           { ELSE }
  | "let"            { LET }
  | "in"             { IN }
  | "rec"            { REC }
  | "ref"            { REF }
  | "()"             { UNIT }

  (*Constantes numeriques*)
  | ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }

  (*Identificateurs*)
  | ['a'-'z''_']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }

  | eof              { EOF }

  | _ as char        {
    let pos = lexbuf.lex_curr_p in
    let msg = Printf.sprintf "Lexing error: unexpected character '%c' at line %d, position %d"
                             char pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) in
    raise (Failure msg)
  }
