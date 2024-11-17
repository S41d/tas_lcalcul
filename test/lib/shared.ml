open Lib.Eval
open Lib.Type

(* testables *)
let pterm_test_eq = Alcotest.testable pp_pterm equal_pterm
let pterm_test_neq = Alcotest.testable pp_pterm (fun t1 t2 -> not (equal_pterm t1 t2))

let ptype_test_eq = Alcotest.testable pp_ptype equal_ptype

(* vars - too lazy to write "" everytime *)
let x = "x"
and y = "y"
and z = "z"
and n = "n"
and m = "m"
and f = "f"
and e = "e"
and a = "a"
and b = "b"
and c = "c"
and d = "d"
and us = "_"

let test_eval actual expected () = Alcotest.check pterm_test_eq "equal" expected (ltr_cbv_norm actual)
let test_type actual expected env () = Alcotest.check ptype_test_eq "equal" expected (actual |> infer_type env |> Option.get)
