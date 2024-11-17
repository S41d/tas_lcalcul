open Lib.Ast
open Lib.Eval
open Shared

let test_add_one_two        = test_eval (Add (Int 1, Int 2))                         (Int 3)
let test_add_commutative    = test_eval (Add (Int 3, Int 5))                         (ltr_cbv_norm (Add (Int 5, Int 3)))

let test_sub_one_two        = test_eval (Sub (Int 1, Int 2))                         (Int (-1))
let test_sub_commutative    = test_eval (Sub (Int (-3), Int 5))                      (ltr_cbv_norm (Sub (Int (-5), Int 3)))

let test_mul_one_two        = test_eval (Mul (Int 1, Int 2))                         (Int 2)
let test_mul_commutative    = test_eval (Mul (Int 3, Int 5))                         (ltr_cbv_norm (Mul (Int 5, Int 3)))
let test_mul_two_three      = test_eval (Mul (Int 2, Int 3))                         (Int 6)

let test_cons_one           = test_eval (Cons (Int 1, Nil))                          (TList [Int 1])
let test_hd_cons_two        = test_eval (Hd (Cons (Int 2, Nil)))                     (Int 2)
let test_tl_cons_two        = test_eval (Tl (Cons (Int 2, Nil)))                     (TList [])
let test_hd_cons_three_two  = test_eval (Hd (Cons (Int 3, Cons (Int 2, Nil))))       (Int 3)
let test_tl_cons_two_three  = test_eval (Tl (Cons (Int 2, Cons (Int 3, Nil))))       (TList [Int 3])

let test_ifz_true           = test_eval (IfZero (Int 0, Int 1, Int 2))               (Int 1)
let test_ifz_false          = test_eval (IfZero (Int 1, Int 1, Int 2))               (Int 2)

let test_ife_true           = test_eval (IfEmpty (Nil, Int 1, Int 2))                (Int 1)
let test_ife_false          = test_eval (IfEmpty (Cons (Int 1, Nil), Int 1, Int 2))  (Int 2)

let test_let_simple         = test_eval (Let (x, Int 1, Var x))                      (Int 1)

let test_factorial =
  let f = "factorial" in
  let fact = Fix (Abs (f, Abs (n, IfZero (Var n, Int 1, Mul (Var n, App (Var f, Sub (Var n, Int 1))))))) in
  test_eval (App (fact, Int 2)) (Int 2)
;;

let () =
  let open Alcotest in
  run "4 - Eval" [
    "id", [ test_case "int 1" `Quick (test_eval (Int 1) (Int 1)) ];

    "add", [
      test_case "add (int 1) (int 2) = 3" `Quick test_add_one_two;
      test_case "add (int 3) (int 5) = add (int 5) (int 3)" `Quick test_add_commutative
    ];

    "sub", [
      test_case "sub (int 1) (int 2) = -1" `Quick test_sub_one_two;
      test_case "sub (int -3) (int 5) = sub (int -5) (int 3)" `Quick test_sub_commutative
    ];

    "mul", [
      test_case "mul (int 1) (int 2) = 2" `Quick test_mul_one_two;
      test_case "mul (int 3) (int 5) = mul (int 5) (int 3)" `Quick test_mul_commutative;
      test_case "mul (int 2) (int 3) = 6" `Quick test_mul_two_three;
    ];

    "cons", [ test_case "cons (1, nil) = [1]" `Quick test_cons_one; ];
    "hd", [
      test_case "hd (cons (2, nil)) = 2" `Quick test_hd_cons_two;
      test_case "hd (cons (3, cons (2, nil))) = 3" `Quick test_hd_cons_three_two
    ];
    "tl", [
      test_case "tl (cons (2, nil)) = []" `Quick test_tl_cons_two;
      test_case "tl (cons (2, cons (3, nil))) = [3]" `Quick test_tl_cons_two_three;
    ];

    "if zero", [
      test_case "ifz (int 0) (int 1) (int 2) = 1" `Quick test_ifz_true;
      test_case "ifz (int 1) (int 1) (int 2) = 2" `Quick test_ifz_false;
    ];

    "if empty", [
      test_case "ife nil (int 1) (int 2) = 1" `Quick test_ife_true;
      test_case "ife (cons (int 1, nil)) (int 1) (int 2) = 2" `Quick test_ife_false;
    ];

    "let", [ test_case "let x = 1 in x" `Quick test_let_simple ];

    "factorial 5 = 120", [ test_case "factorial (int 5) = 120" `Quick test_factorial ];
  ]
;;
