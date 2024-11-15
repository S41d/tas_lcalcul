open Lib.Eval

let () =
  let l = IfEmpty (Nil, Int 1, Int 2) |> ltr_cbv_norm in
  show_pterm l |> print_endline
;;
