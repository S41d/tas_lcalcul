open Lib.Eval
(* open Lib.Type *)

let () =
  let fact = Fix (Abs ("f", Abs ("n", IfZero (Var "n", Int 1, Mul (Var "n", App (Var "f", Sub (Var "n", Int 1))))))) in
  let t = App (fact, Int 2) |> ltr_cbv_norm_timeout 20 in
  show_pterm t |> print_endline
;;
