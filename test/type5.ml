open Lib.Type
open Lib.Eval
open Shared

let test_unit             = test_type Unit                                                                     Unit       []

let test_ref_creation     = test_type (Ref Unit)                                                               (Ref Unit) []
let test_ref_int          = test_type (Ref (Int 42))                                                           (Ref Nat)  []

let test_deref            = test_type (Deref (Ref (Int 42)))                                                   Nat        []
let test_deref_unit       = test_type (Deref (Ref Unit))                                                       Unit       []

let test_assign           = test_type (Assign (Ref (Int 1), Int 2))                                            Unit       []
let test_assign_and_deref = test_type (Let (x, Ref (Int 1), Let (us, Assign (Var x, Int 2), Deref (Var x))))   (Nat)      []

let () =
  let open Alcotest in
  run "5 - Type" [
    "unit", [
      test_case "() = unit" `Quick test_unit;
    ];
    "ref", [
      test_case "ref () = ref unit" `Quick test_ref_creation;
      test_case "ref 42 = ref nat" `Quick test_ref_int;
    ];
    "deref", [
      test_case "deref (ref 42) = nat" `Quick test_deref;
      test_case "deref (ref ()) = unit" `Quick test_deref_unit;
    ];
    "assignment", [
      test_case "assign (ref 1, 2) = unit" `Quick test_assign;
      test_case "assign and deref" `Quick test_assign_and_deref;
    ]
  ]
;;
