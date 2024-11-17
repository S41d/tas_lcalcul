open Lib.Parser
open Lib.Lexer
open Lib.Eval
open Lib.Type

let () =
  let fname = Sys.argv.(1) in
  let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p =
      try prog token lexbuf with
      | Parsing.Parse_error ->
        let open Lexing in
        let curr = lexbuf.lex_curr_p in
        let line = curr.pos_lnum in
        let cnum = curr.pos_cnum - curr.pos_bol in
        Printf.printf "Erreur de syntaxe à la ligne %d, colonne %d\n" line cnum;
        exit 1
    in
    print_newline ();
    print_endline "Type Checking of :";
    p |> show_pterm |> print_endline;
    print_newline ();
    match p |> infer_type [] with
    | Some t -> print_endline ("Result: " ^ show_ptype t)
    | None -> print_endline "Type checking failed"
  with
  | Eof -> Printf.printf "Erreur : fin de fichier inattendue.\n"
;;
