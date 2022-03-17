open OUnit2
open Sectool
open Sectool.Elliptic_curve
open Sectool.File_wizard
open Sectool.Cryptography

(********************************************************************
   Here are some helper functions for your testing of set-like lists.
 ********************************************************************)

(* Checks that scaling a point by n works the same as adding it to itself
n times *)

(**
let multiply_add_test name f n p =
  let rec repeated_add (n : Z.t) p1 =
    if n = Z.zero then p1 else add_points f p1 (repeated_add (n - Z.one) p1)
  in
  name >:: fun _ -> assert_equal (repeated_add n p) (multiply_point f n p)
*)

let read_private_key_test
    (name : string)
    (input : string)
    (expected_output: int) =
  name >:: fun _ ->
  assert_equal ~printer:string_of_int expected_output (read_private_key input)

let read_public_key_test
    (name : string)
    (input : string)
    (expected_output: Elliptic_curve.point) =
  name >:: fun _ ->
  assert_equal ~printer:string_of_point expected_output (read_public_key input)

let read_domain_params_test
    (name : string)
    (input : string)
    (expected_output: Elliptic_curve.field) =
  name >:: fun _ ->
  assert_equal ~printer:string_of_field expected_output (read_domain_params input)


let fio_tests = [
  read_private_key_test "test" "private_key_1" 100;
  read_public_key_test "test" "public_key_1" { x=50 ; y=47 };
  read_domain_params_test "test" "field_1" { p=2 ; a=1 ; b=486662 ; c=1 ; d=0 ; g=9 ; n=252 ; h=8 };
]

let ecc_tests = [

]

let aes_tests = [

]

let suite =
  "test suite for A2"
  >::: List.flatten [ ecc_tests; aes_tests; fio_tests ]

let _ = run_test_tt_main suite
