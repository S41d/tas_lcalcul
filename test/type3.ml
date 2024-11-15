open Alcotest
open Lib.Type
open Lib.Eval
open Shared

let create_env pairs =
  List.map (fun (name, typ) -> (name, typ)) pairs

let test_infer_nat () =
  let result = infer_type (Int 42) [] in
  match result with
  | Some typ -> check bool "should be Nat" true (equal_ptype typ Nat)
  | None -> fail "Expected Some type, got None"

let () =
  let open Alcotest in
  run
    "3 - Type"
    [ "int var", [ test_case "var_int" `Quick test_infer_nat ]
    ]
;;
