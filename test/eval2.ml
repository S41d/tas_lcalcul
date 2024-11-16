open Lib.Eval
open Shared
open Alcotest

let i = Abs (x, Var x)
let delta = Abs (x, App (Var x, Var x))
let s = Abs (x, Abs (y, Abs (z, App (App (Var x, Var z), App (Var y, Var z)))))
let k = Abs (x, Abs (y, Var x))

let one = Abs (f, Abs (e, App (Var f, Var e)))
let two = Abs (f, Abs (e, App (Var f, App (Var f, Var e))))
let three = Abs (f, Abs (e, App (Var f, App (Var f, App (Var f, Var e)))))
let four = Abs (f, Abs (e, App (Var f, App (Var f, App (Var f, App (Var f, Var e))))))

let add = Abs(n, Abs(m, Abs(f, Abs(e, App(App(Var m, Var f), App(App(Var n, Var f), Var e))))))
let mul = Abs(n, Abs(m, Abs(f, Abs(e, App(App(Var n, App(Var m, Var f)), Var e)))))
let pow = Abs(n, Abs(m, App(Var m, Var n)))

let test actual expected () =
  check pterm_test_eq "equal" expected (ltr_cbv_norm actual)
;;

let test_II           = test (App (i, i))                  i
let test_SII          = test (App (App (s, i), i))         (Abs (y, App (Var y, Var y)))
let test_SKK          = test (App (App (s, k), k))         i
let test_deltaII      = test (App (delta, App (i, i)))     i
let test_plus_one_two = test (App (App (add, one), two))   three
let test_plus_two_two = test (App (App (add, two), two))   four
let test_mul_one_two  = test (App (App (mul, one), two))   two
let test_mul_two_two  = test (App (App (mul, two), two))   four
let test_pow_two_two  = test (App (App (pow, two), two))   four

let () =
  let open Alcotest in
  run
    "2 - Eval"
    [ "II", [ test_case (show_pterm (App (i, i))) `Quick test_II ]
    ; "add 1 2", [ test_case (show_pterm (App (App (add, one), two))) `Quick test_plus_one_two ]
    ; "add 2 2", [ test_case (show_pterm (App (App (add, two), two))) `Quick test_plus_two_two ]
    ; "mul 1 2", [ test_case (show_pterm (App (App (mul, one), two))) `Quick test_mul_one_two ]
    ; "mul 2 2", [ test_case (show_pterm (App (App (mul, two), two))) `Quick test_mul_two_two ]
    ; "pow 2 2", [ test_case (show_pterm (App (App (pow, two), two))) `Quick test_pow_two_two ]
    ; "SKK", [ test_case (show_pterm (App (App (s, k), k))) `Quick test_SKK ]
    ; "SII", [ test_case (show_pterm (App (App (s, i), i))) `Quick test_SII ]
    ; "deltaII", [ test_case "Delta I I" `Quick test_deltaII ]
    ]
;;
