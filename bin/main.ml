open Lib.Eval
(* open Lib.Type *)

let () =
  let t = (Let ("x", Ref (Int 1), Let ("_", Assign (Var "x", Int 2), Deref (Var "x")))) |> ltr_cbv_norm in
  show_pterm t |> print_endline
;;
