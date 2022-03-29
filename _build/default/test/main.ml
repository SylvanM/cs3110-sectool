open OUnit2
open Sectool
open ED25519
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

let same_secret_test name d1 d2 = 
  let p1 = compute_public_key d1 in 
  let p2 = compute_public_key d2 in 
  let s1 = compute_shared_secret d1 p2 in 
  let s2 = compute_shared_secret d2 p1 in 
  name >:: fun _ -> assert_equal s1 s2 ~printer:string_of_point
  
(** TESTS  *)

let ecc_tests = [

  same_secret_test "Basic key test" (234 |> Z.of_int) (75463 |> Z.of_int)

]

let aes_tests = [

]

let suite =
  "test suite for A2"
  >::: List.flatten [ ecc_tests; aes_tests; ]

let _ = run_test_tt_main suite
