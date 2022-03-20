open OUnit2
open Sectool
open Sectool.Elliptic_curve
open Z
open Testing_constants
open Ecdh

(********************************************************************
   Here are some helper functions for your testing of set-like lists.
 ********************************************************************)

(* Checks that scaling a point by n works the same as adding it to itself
n times *)
let multiply_add_test name f n p =
  let rec repeated_add (n : Z.t) p1 =
    if n = Z.zero then p1 else add_points f p1 (repeated_add (n - Z.one) p1)
  in
  name >:: fun _ -> assert_equal (repeated_add n p) (multiply_point f n p)

let same_secret_test name f d1 d2 = 
  let p1 = compute_public_key f d1 in 
  let p2 = compute_public_key f d1 in 
  let s1 = compute_shared_secret f d1 p2 in 
  let s2 = compute_shared_secret f d2 p1 in 
  name >:: fun _ -> assert_equal s1 s2

let add_test name f p1 p2 p3 =
  let sum = add_points f p1 p2 in 
  name >:: fun _ -> assert_equal p3 sum ~printer:string_of_point

let double_test name f p dp =
  let dub = multiply_point f (Z.of_int 2) p in 
  name >:: fun _ -> assert_equal dp dub ~printer:string_of_point

let modulus_test name f expected_m = 
  let m = f |> get_modulus in 
  name >:: fun _ -> assert_equal expected_m m ~printer:Z.to_string

let fio_tests = [
  
]

let ecc_tests = [
  (* same_secret_test "Simple Curve 1 and 2" simple_curve Z.one (2 |> Z.of_int) ; *)
  (* same_secret_test "25519 1 and 2" curve25519 Z.one (2 |> Z.of_int) ; *)

  add_test "(1, 6) + (4, 6)" example_curve (make_point 1 6) (make_point 4 6) (make_point 8 7);
  double_test "2 * (1, 6)" example_curve (make_point 1 6) (make_point 10 1);
]

let aes_tests = [

]

let suite =
  "test suite for A2"
  >::: List.flatten [ ecc_tests; aes_tests; fio_tests ]

let _ = run_test_tt_main suite
