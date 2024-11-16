open Alcotest
open Lib.Type
open Lib.Eval
open Shared

let test actual expected env () =
  check ptype_test_eq "equal" expected (actual |> infer_type env |> Option.get)

let test_add = test (Add (Int 1, Int 2)) Nat []
let test_add_var = test (Add (Int 1, Var x)) Nat [(x, Nat)]
let test_sub = test (Sub (Int 1, Int 2)) Nat []
let test_sub_var = test (Sub (Int 1, Var x)) Nat [(x, Nat)]

let test_list_nat_1 = test (Cons (Int 1, Nil)) (TList Nat) []
let test_list_nat_2 = test (Cons (Int 1, Cons (Int 2, Nil))) (TList Nat) []

let test_let_1 = test (Let (x, Int 1, Var x)) Nat []
let test_let_2 =
  let fn = Abs (x, (Let (y, Add (Var x, Int 2), (Sub (Var x, Var y))))) in
  test (App (fn, Int 4)) Nat []

let test_let_3 =
  let term = Let (f, Abs (x, Add (Var x, Int 2)), App (Var f, Int 4)) in
  test term Nat []

let test_let_4 =
  let term = Let (f, Abs (x, App (Var x, Int 2)), Var f) in
  test term (Arr (Arr (Nat, Var x), Var x)) []

let () =
  let open Alcotest in
  run
    "4 - Type"
    [
      "add", [ test_case (show_ptype (Nat)) `Quick test_add];
      "add with var", [ test_case (show_ptype (Nat)) `Quick test_add_var];
      "sub", [ test_case (show_ptype (Nat)) `Quick test_sub];
      "sub with var", [ test_case (show_ptype (Nat)) `Quick test_sub_var];
      "list nat 1", [ test_case (show_ptype (TList Nat)) `Quick test_list_nat_1];
      "list nat 2", [ test_case (show_ptype (TList Nat)) `Quick test_list_nat_2];
      "let 1", [ test_case (show_ptype (Nat)) `Quick test_let_1];
      "let 2", [ test_case (show_ptype (Nat)) `Quick test_let_2];
      "let 3", [ test_case (show_ptype (Nat)) `Quick test_let_3];
      "let 4", [ test_case (show_ptype (Arr (Arr (Nat, Var x), Var x))) `Quick test_let_4];
    ]
;;
