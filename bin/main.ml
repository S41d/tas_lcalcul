open Lib.Eval

let () =
  let t = Abs ("f", Abs ("g", App (Var "f", Var "g"))) in
  t |> show_pterm |> print_endline;
  t |> alphaconv |> show_pterm |> print_endline;
;;
