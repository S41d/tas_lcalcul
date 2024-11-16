open Lib.Eval
open Lib.Type

let () =
  let t = (Cons (Int 1, Nil)) |> infer_type [] |> Option.get in
  show_ptype t |> print_endline
;;
