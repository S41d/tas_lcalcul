open Lib.Parser
open Lib.Lexer
open Lib.Eval

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
    print_endline "Evaluation of :";
    p |> show_pterm |> print_endline;
    print_newline ();
    print_endline "Result :";
    p |> ltr_cbv_norm |> show_pterm |> print_endline;
  with
  | Eof -> Printf.printf "Erreur : fin de fichier inattendue.\n"
;;
