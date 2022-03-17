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

let fio_tests = [
  
]

let ecc_tests = [
  same_secret_test "25519 1 and 2" curve25519 Z.one (2 |> Z.of_int) ;
  same_secret_test "25519 p and q" curve25519 (Z.of_string_base 16 "afe172371") 
    (Z.of_string_base 16 "12432ff") ;
]

let aes_tests = [

]

let suite =
  "test suite for A2"
  >::: List.flatten [ ecc_tests; aes_tests; fio_tests ]

let _ = run_test_tt_main suite
