open Lib.Eval
open Lib.Type

let () =
  let t = Abs ("x", Abs("y", Var "x")) |> infer_type [] |> Option.get in
  show_ptype t |> print_endline
;;
