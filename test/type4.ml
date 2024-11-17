open Lib.Ast
open Lib.Eval
open Lib.Type
open Shared

let test_add =        test_type (Add (Int 1, Int 2))               Nat         []
let test_add_var =    test_type (Add (Int 1, Var x))               Nat         [(x, Nat)]
let test_sub =        test_type (Sub (Int 1, Int 2))               Nat         []
let test_sub_var =    test_type (Sub (Int 1, Var x))               Nat         [(x, Nat)]

let test_list_nat_1 = test_type (Cons (Int 1, Nil))                (TList Nat) []
let test_list_nat_2 = test_type (Cons (Int 1, Cons (Int 2, Nil)))  (TList Nat) []

let test_let_1 =      test_type (Let (x, Int 1, Var x))            Nat         []
let test_let_2 =
  let fn = Abs (x, (Let (y, Add (Var x, Int 2), (Sub (Var x, Var y))))) in
  test_type (App (fn, Int 4)) Nat []
;;

let test_let_3 =
  let term = Let (f, Abs (x, Add (Var x, Int 2)), App (Var f, Int 4)) in
  test_type term Nat []
;;

let test_let_4 =
  let term = Let (f, Abs (x, App (Var x, Int 2)), Var f) in
  let expected = Arr (Arr (Nat, Var x), Var x) in
  test_type term expected []
;;

let test_factorial =
  let fact = Fix (Abs ("f", Abs ("n", IfZero (Var "n", Int 1, Mul (Var "n", App (Var "f", Sub (Var "n", Int 1))))))) in
  let term = App (fact, Int 2) |> ltr_cbv_norm in
  test_type term Nat []
;;

let () =
  let open Alcotest in
  run
    "4 - Type"
    [
      "add", [
        test_case (show_ptype (Nat)) `Quick test_add;
        test_case (show_ptype (Nat)) `Quick test_add_var
      ];
      "sub", [
        test_case (show_ptype (Nat)) `Quick test_sub;
        test_case (show_ptype (Nat)) `Quick test_sub_var
      ];
      "list nat", [
        test_case (show_ptype (TList Nat)) `Quick test_list_nat_1;
        test_case (show_ptype (TList Nat)) `Quick test_list_nat_2
      ];
      "let", [
        test_case (show_ptype (Nat)) `Quick test_let_1;
        test_case (show_ptype (Nat)) `Quick test_let_2;
        test_case (show_ptype (Nat)) `Quick test_let_3;
        test_case (show_ptype (Arr (Arr (Nat, Var a), Var a))) `Quick test_let_4 
      ];
      "factorial", [
        test_case "factorial 5 = Nat" `Quick test_factorial;
      ];
    ]
;;
