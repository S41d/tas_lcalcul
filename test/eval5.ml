open Alcotest
open Lib.Eval
open Shared

let test actual expected () =
  check pterm_test_eq "equal" expected (ltr_cbv_norm actual)
;;

let test_unit             = test Unit                                                                     Unit

let test_ref_creation     = test (Ref Unit)                                                               (Region 1)
let test_ref_int          = test (Ref (Int 42))                                                           (Region 1)

let test_deref            = test (Deref (Ref (Int 42)))                                                   (Int 42)
let test_deref_unit       = test (Deref (Ref Unit))                                                       Unit

let test_assign           = test (Assign (Ref (Int 1), Int 2))                                            Unit
let test_assign_and_deref = test (Let (x, Ref (Int 1), Let (us, Assign (Var x, Int 2), Deref (Var x))))   (Int 2)

let () =
  run "Evaluation Tests" [
    "unit", [
      test_case "unit value" `Quick test_unit;
    ];
    "ref", [
      test_case "ref creation with unit" `Quick test_ref_creation;
      test_case "ref creation with int" `Quick test_ref_int;
    ];
    "deref", [
      test_case "deref int" `Quick test_deref;
      test_case "deref unit" `Quick test_deref_unit;
    ];
    "assignment", [
      test_case "simple assignment" `Quick test_assign;
      test_case "assign and deref" `Quick test_assign_and_deref;
    ];
  ]
;;
