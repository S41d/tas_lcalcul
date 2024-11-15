open Lib.Eval
open Shared

let i = Abs (x, Var x)
let delta = Abs (x, App (Var x, Var x))
let s = Abs (x, Abs (y, Abs (z, App (App (Var x, Var z), App (Var y, Var z)))))
let k = Abs (x, Abs (y, Var x))

let one = Abs (f, Abs (e, App (Var f, Var e)))
let two = Abs (f, Abs (e, App (Var f, App (Var f, Var e))))
let three = Abs (f, Abs (e, App (Var f, App (Var f, App (Var f, Var e)))))
let four = Abs (f, Abs (e, App (Var f, App (Var f, App (Var f, App (Var f, Var e))))))

let add = Abs(n, Abs(m, Abs(f, Abs(e, App(App(Var n, Var f), App(App(Var m, Var f), Var e))))))
let mul = Abs(n, Abs(m, Abs(f, Abs(e, App(App(Var n, App(Var m, Var f)), Var e)))))
let pow = Abs(n, Abs(m, Abs(f, Abs(e, App(App(App(Var m, Var n), Var f),Var e)))))

let test_ii () =
  let t = App (i, i) |> ltr_cbv_norm in
  Alcotest.check pterm_test_eq "equal" i t
;;

let test_SII () =
  let actual = App (App(s, i), i) |> ltr_cbv_norm in
  let expected = Abs (y, App (Var y, Var y)) in
  Alcotest.check pterm_test_eq "equal" expected actual
;;

let test_SKK () =
  let actual = App (App (s, k), k) |> ltr_cbv_norm in
  Alcotest.check pterm_test_eq "equal" i actual
;;

let test_deltaII () =
  let actual = App (delta, App (i, i)) |> ltr_cbv_norm in
  Alcotest.check pterm_test_eq "equal" i actual
;;

let test_plus_one_two () =
  let actual = App (App (add, one), two) |> ltr_cbv_norm in
  Alcotest.check pterm_test_eq "equal" three actual
;;

let test_plus_two_two () =
  let actual = App (App (add, two), two) |> ltr_cbv_norm in
  Alcotest.check pterm_test_eq "equal" four actual
;;

let test_mul_one_two () =
  let actual = App (App (mul, one), two) |> ltr_cbv_norm in
  Alcotest.check pterm_test_eq "equal" two actual
;;

let test_mul_two_two () =
  let actual = App (App (mul, two), two) |> ltr_cbv_norm in
  Alcotest.check pterm_test_eq "equal" four actual
;;

let test_pow_two_two () =
  let actual = App (App (pow, two), two) |> ltr_cbv_norm in
  Alcotest.check pterm_test_eq "equal" four actual
;;

let () =
  let open Alcotest in
  run
    "2 - Eval"
    [ "II", [ test_case "(\\x.x)(\\x.x)" `Quick test_ii ]
    ; "1+2", [ test_case "add one two" `Quick test_plus_one_two ]
    ; "2+2", [ test_case "add two two" `Quick test_plus_two_two ]
    ; "1x2", [ test_case "mul two two" `Quick test_mul_one_two ]
    ; "2x2", [ test_case "mul two two" `Quick test_mul_two_two ]
    ; "2^2", [ test_case "pow two two" `Quick test_pow_two_two ]
    ; "SKK", [ test_case "(\\xyz.(x z) (y z))(\\x1y.x1)(\\x2y.x2)" `Quick test_SKK ]
    ; "SII", [ test_case "(\\xyz.(x z) (y z))(\\x1.x1)(\\x2.x2)" `Quick test_SII ]
    ; "deltaII", [ test_case "Delta I I" `Quick test_deltaII ]
    ]
;;
