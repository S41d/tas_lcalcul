open Lib.Eval
open Shared

let test_unit             = test_eval Unit                                                                     Unit

let test_ref_creation     = test_eval (Ref Unit)                                                               (Region 1)
let test_ref_int          = test_eval (Ref (Int 42))                                                           (Region 1)

let test_deref            = test_eval (Deref (Ref (Int 42)))                                                   (Int 42)
let test_deref_unit       = test_eval (Deref (Ref Unit))                                                       Unit

let test_assign           = test_eval (Assign (Ref (Int 1), Int 2))                                            Unit
let test_assign_and_deref = test_eval (Let (x, Ref (Int 1), Let (us, Assign (Var x, Int 2), Deref (Var x))))   (Int 2)

let () =
  let open Alcotest in
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
