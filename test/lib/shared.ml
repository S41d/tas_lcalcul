open Lib.Eval
open Lib.Type

(* testables *)
let pterm_test_eq = Alcotest.testable pp_pterm equal_pterm
let pterm_test_neq = Alcotest.testable pp_pterm (fun t1 t2 -> not (equal_pterm t1 t2))

let ptype_test_eq = Alcotest.testable pp_ptype equal_ptype

(* vars *)
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
