open Lib.Ast
open Lib.Type
open Shared

let test_arr_1 = test_type (Abs (x, Var x))          (Arr (Var x, Var x))                []
let test_arr_2 = test_type (Abs (x, Abs(y, Var x)))  (Arr (Var x, Arr (Var y, Var x)))   []

let test_var1  = test_type (Var x)                   Nat                                 [(x, Nat)]
let test_var2  = test_type (Var x)                   (Arr (Var y, Var y))                [(x, Arr (Var y, Var y))]

let () =
  let open Alcotest in
  run "3 - Type" [
    "var", [
      test_case (show_ptype (Var x)) `Quick test_var1;
      test_case (show_ptype (Var x)) `Quick test_var2
    ];
    "arrow", [
      test_case (show_ptype (Arr (Var x, Var x))) `Quick test_arr_1;
      test_case (show_ptype (Arr (Arr (Var x, Var y), Var x))) `Quick test_arr_2
    ];
  ]
;;
