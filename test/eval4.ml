open Alcotest
open Lib.Eval
open Shared

let test actual expected () =
  check pterm_test_eq "equal" expected (ltr_cbv_norm actual)
;;

let test_add_one_two        = test (Add (Int 1, Int 2))                         (Int 3)
let test_add_commutative    = test (Add (Int 3, Int 5))                         (ltr_cbv_norm (Add (Int 5, Int 3)))

let test_sub_one_two        = test (Sub (Int 1, Int 2))                         (Int (-1))
let test_sub_commutative    = test (Sub (Int (-3), Int 5))                      (ltr_cbv_norm (Sub (Int (-5), Int 3)))

let test_cons_one           = test (Cons (Int 1, Nil))                          (TList [Int 1])
let test_hd_cons_two        = test (Hd (Cons (Int 2, Nil)))                     (Int 2)
let test_tl_cons_two        = test (Tl (Cons (Int 2, Nil)))                     Nil
let test_hd_cons_three_two  = test (Hd (Cons (Int 3, Cons (Int 2, Nil))))       (Int 3)
let test_tl_cons_two_three  = test (Tl (Cons (Int 2, Cons (Int 3, Nil))))       (TList [Int 3])

let test_ifz_true           = test (IfZero (Int 0, Int 1, Int 2))               (Int 1)
let test_ifz_false          = test (IfZero (Int 1, Int 1, Int 2))               (Int 2)

let test_ife_true           = test (IfEmpty (Nil, Int 1, Int 2))                (Int 1)
let test_ife_false          = test (IfEmpty (Cons (Int 1, Nil), Int 1, Int 2))  (Int 2)

let test_fix                = test (Fix (Abs ("x", Var "x")))                   (Fix (Abs ("x", Var "x")))
let test_let_simple         = test (Let ("x", Int 1, Var "x"))                  (Int 1)

let () =
  let open Alcotest in
  run
    "4 - Eval"
    [
      "1 = 1", [ test_case "int 1" `Quick (test (Int 1) (Int 1)) ];

      "1 + 2 = 3", [ test_case "add (int 1) (int 2)" `Quick test_add_one_two ];
      "3 + 5 = 5 + 3", [ test_case "add (int 3) (int 5) = add (int 5) (int 3)" `Quick test_add_commutative ];

      "1 - 2 = -1", [ test_case "sub (int 1) (int 2)" `Quick test_sub_one_two ];
      "-3 - 5 = -5 - 3", [ test_case "sub (int -3) (int 5) = sub (int -5) (int 3)" `Quick test_sub_commutative ];

      "cons 1::[] = [1]", [ test_case "cons (1, nil)" `Quick test_cons_one ];
      "hd 2::[] = 2", [ test_case "hd (cons (2, nil))" `Quick test_hd_cons_two ];
      "tl 2::[] = []", [ test_case "tl (cons (2, nil))" `Quick test_hd_cons_two ];
      "hd 3::2::[] = 3", [ test_case "hd (cons (3, cons (2, nil)))" `Quick test_hd_cons_three_two ];
      "tl 2::3::[] = [3]", [ test_case "tl (cons (2, cons (3, nil)))" `Quick test_tl_cons_two_three ];

      "ifz 0 then 1 else 2 = 1", [ test_case "ifz (int 0) (int 1) (int 2)" `Quick test_ifz_true ];
      "ifz 1 then 1 else 2 = 2", [ test_case "ifz (int 1) (int 1) (int 2)" `Quick test_ifz_false ];
      "ife [] then 1 else 2 = 1", [ test_case "ife nil (int 1) (int 2)" `Quick test_ife_true ];
      "ife 1::[] then 1 else 2 = 2", [ test_case "ife (cons (int 1, nil)) (int 1) (int 2)" `Quick test_ife_false ];

      "fix (\\x.x) = fix (\\x.x)", [ test_case "fix (abs (x, var x))" `Quick test_fix ];
      "let x = 1 in x = 1", [ test_case "let x = int 1 in var x" `Quick test_let_simple ];
    ]
;;
