open Alcotest
open Lib.Type
open Lib.Eval
open Shared

let test actual expected env () =
  check ptype_test_eq "equal" expected (actual |> infer_type env |> Option.get)
;;

let test_arr_1 = test (Abs (x, Var x))          (Arr (Var x, Var x))                []
let test_arr_2 = test (Abs (x, Abs(y, Var x)))  (Arr (Var x, Arr (Var y, Var x)))   []

let test_var1  = test (Var x)                   Nat                                 [(x, Nat)]
let test_var2  = test (Var x)                   (Arr (Var y, Var y))                [(x, Arr (Var y, Var y))]

let () =
  let open Alcotest in
  run
    "3 - Type"
    [
      "var 1", [ test_case (show_ptype (Var x)) `Quick test_var1];
      "var 2", [ test_case (show_ptype (Var x)) `Quick test_var2];
      "arrow 1", [ test_case (show_ptype (Arr (Var x, Var x))) `Quick test_arr_1];
      "arrow 2", [ test_case (show_ptype (Arr (Arr (Var x, Var y), Var x))) `Quick test_arr_2];
    ]
;;
