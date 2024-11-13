open Lib.Eval

(* testables *)
let pterm_test_eq = Alcotest.testable pp_pterm equal_pterm
let pterm_test_neq = Alcotest.testable pp_pterm (fun t1 t2 -> not (equal_pterm t1 t2))

(* vars *)
let x = "x"
let y = "y"
let z = "z"

(* fn *)
let i = Abs (x, Var x)
let delta = Abs (x, App (Var x, Var x))
let s = Abs (x, Abs (y, Abs (z, App (App (Var x, Var z), App (Var y, Var z)))))
let k = Abs (x, Abs (y, Var x))

let test_eq_II () =
  let t = App (i, i) |> ltr_cbv_norm in
  Alcotest.check pterm_test_eq "equal" i t
;;

let test_eq_SII () =
  let t = App (s, App (i, i)) |> ltr_cbv_norm in
  Alcotest.check pterm_test_eq "equal" i t
;;

let test_eq_SKK () =
  let t = App (s, App (k, k)) |> ltr_cbv_norm in
  Alcotest.check pterm_test_eq "equal" i t
;;

let test_eq_dII () =
  let t = App (delta, App (i, i)) |> ltr_cbv_norm in
  Alcotest.check pterm_test_eq "equal" i t
;;

let test_neq_pterm () =
  let term1 = Var "x" in
  let term2 = App (Var "x", Abs ("y", Var "y")) in
  Alcotest.check pterm_test_neq "unequal pterm" term1 term2
;;

let () =
  let open Alcotest in
  run
    "Pterm tests"
    [ "II", [ test_case "(\\x.x)(\\x.x)" `Quick test_eq_II ]
    ; "SKK", [ test_case "(\\xyz.(x z) (y z))(\\x1y.x1)(\\x2y.x2)" `Quick test_eq_SKK ]
    ; "SII", [ test_case "(\\xyz.(x z) (y z))(\\x1.x1)(\\x2.x2)" `Quick test_eq_SII ]
    ; "deltaII", [ test_case "compares pterm for equality" `Quick test_eq_dII ]
    ; "Neq pterm", [ test_case "compares pterm for inequality" `Quick test_neq_pterm ]
    ]
;;
