open OUnit2
open Sectool
open Sectool.Elliptic_curve
open Testing_constants
open Ecdh
open Sectool.File_wizard

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

let same_secret_test name f d1 d2 = 
  let p1 = compute_public_key f d1 in 
  let p2 = compute_public_key f d2 in 
  let s1 = compute_shared_secret f d1 p2 in 
  let s2 = compute_shared_secret f d2 p1 in 
  name >:: fun _ -> assert_equal s1 s2 ~printer:string_of_point

let add_test name f p1 p2 p3 =
  let sum = add_points f p1 p2 in 
  name >:: fun _ -> assert_equal p3 sum ~printer:string_of_point

let double_test name f p dp =
  let dub = multiply_point f (Z.of_int 2) p in 
  name >:: fun _ -> assert_equal dp dub ~printer:string_of_point

let multiply_test name f p n correct = 
  let prod = multiply_point f (Z.of_int n) p in 
  name >:: fun _ -> assert_equal correct prod ~printer:string_of_point

let read_private_key_test
    (name : string)
    (input : string)
    (expected_output: Z.t) =
  name >:: fun _ ->
  assert_equal ~printer:Z.to_string expected_output (read_private_key input)

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

(** CONSTANTS *)

let private_key_1 = read_private_key "private_key_1"
let public_key_1 = read_public_key "public_key_1"
let field_1 = read_domain_params "field_1"

(** TESTS  *)

let fio_tests = [
  read_private_key_test "test" "private_key_1" (100 |> Z.of_int);
  read_public_key_test "test" "public_key_1" (make_int_point (50, 47));
  read_domain_params_test "test" "field_1" heewon_field;
]

let ecc_tests = [

  same_secret_test "Simple Secrets" example_curve (3 |> Z.of_int) (4 |> Z.of_int) ;

  add_test "(1, 6) + (4, 6)" example_curve (make_int_point (1, 6)) (make_int_point (4, 6)) (make_int_point (8, 7));
  double_test "2 * (1, 6)" example_curve (make_int_point (1, 6)) (make_int_point (10, 1));

  add_test "P + Q = R On P-192" p192 p192_P p192_Q p192_R ;

  double_test "Q + Q = 2Q on P-192" p192 p192_Q p192_2Q ;

  multiply_test "4P = P + P + P + P"  p192 p192_P 4 (add_points p192 p192_P (add_points p192 p192_P (add_points p192 p192_P p192_P)));

  same_secret_test "Simple Curve 1 and 2" simple_curve Z.one (2 |> Z.of_int) ;
  same_secret_test "25519 1 and 2" curve25519 Z.one (2 |> Z.of_int) ;
  same_secret_test "25519 0xfe22332 and 0xeefa9" curve25519 ("fe22332" |> Z.of_string_base 16) ("eefa9" |> Z.of_string_base 16) ;
  
  add_test "(1, 6) + (4, 6)" example_curve (make_int_point (1, 6)) (make_int_point (4, 6)) (make_int_point (8, 7));
  double_test "2 * (1, 6)" example_curve (make_int_point (1, 6)) (make_int_point (10, 1));

]

let aes_tests = [

]

let suite =
  "test suite for A2"
  >::: List.flatten [ ecc_tests; aes_tests; fio_tests ]

let _ = run_test_tt_main suite
