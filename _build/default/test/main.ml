open OUnit2
open Sectool
open ED25519
open Testing_constants
open EncodingUtility
open ECDH
open EDDSA
open Sectool.File_wizard

(********************************************************************
   Here are some helper functions for your testing of set-like lists.
 ********************************************************************)

let packing_symmetry_test name list width =
  name >:: fun _ ->
    let len = List.length list in 
    let unpacked = list |> pack width |> unpack width len in 
    assert_equal list unpacked

let message_can_be_verified name message = 
  name >:: fun _ ->
    let priv_key = generate_private_key () in 
    let pub_key = compute_public_key priv_key in 
    let m = message |> encode_string in 
    let signature = sign m priv_key in 
    assert_equal true (verify m pub_key signature)

let make_digest message priv_key =
  let encoded_message = encode_string message in 
  let signature = sign encoded_message priv_key in 
  print_string (signature |> string_of_digest) ; signature

let str_encoding_test name str =
  name >:: fun _ ->
    let decoded = str |> encode_string |> decode_string in 
    assert_equal str decoded ~printer:(fun s -> s)

let byte_encoding_test name str =
  name >:: fun _ ->
    let decoded = str |> str_to_bytes |> bytes_to_str in 
    assert_equal str decoded ~printer:(fun s -> s)

let same_secret_test name d1 d2 = 
  let p1 = compute_public_key d1 in 
  let p2 = compute_public_key d2 in 
  let s1 = compute_shared_secret d1 p2 in 
  let s2 = compute_shared_secret d2 p1 in 
  name >:: fun _ -> assert_equal s1 s2 ~printer:Z.to_string

let communativity_test name dA dB = 
  name >:: fun _ -> 
    assert_equal (dA * (dB * base)) (dB * (dA * base)) 
      ~printer:string_of_point

let associativity_test name a b =
  name >:: fun _ ->
    assert_equal 
      (a * (b * base)) ((Z.mul a b) * base)
      ~printer:string_of_point

let digest_encoding_symmetry_test name message =
  name >:: fun _ ->
    let priv_key = generate_private_key () in 
    let digest = make_digest message priv_key in 
    let decoded = digest |> digest_to_data |> data_to_digest in 
    assert_equal digest decoded ~printer:string_of_digest

let rec communativity_tests count =
  if count = 0 then [] else
    let dA = generate_private_key () in 
    let dB = generate_private_key () in 
    let test = communativity_test ("Commun Test " ^ string_of_int count)
      dA dB in 
    test :: (communativity_tests (count - 1))

let rec square_assoc_tests count =
  if count < 0 then [] else 
    let test = associativity_test 
      ("Sq. Assoc Test: " ^ string_of_int count) 
      (Z.of_int count) (Z.of_int count) in 
    test :: (square_assoc_tests (count - 1))

let rec associativity_tests count =
  if count = 0 then [] else
    let dA = generate_private_key () in 
    let dB = generate_private_key () in 
    let test = communativity_test ("Assoc Test " ^ string_of_int count)
      dA dB in 
    test :: (associativity_tests (count - 1))



  
(** TESTS  *)

let ecc_tests = [

  same_secret_test "Small Key Test" (4 |> Z.of_int) (7 |> Z.of_int) ;

  same_secret_test "Basic Key test" (234 |> Z.of_int) (75463 |> Z.of_int) ;
  same_secret_test "Larger Key Test" ("8347201765604315761045784350143751034561" |> Z.of_string) ("4534825034158085927349874325" |> Z.of_string) ;

] 
@ communativity_tests 100
@ associativity_tests 100
@ square_assoc_tests 555

let eddsa_tests = [
  digest_encoding_symmetry_test "Testing Digest Encoding Symmetry" "Sign me!" ;

  message_can_be_verified "Can verify a simple message" "Verify me!" ;
  message_can_be_verified "Can verify number sring" "8347201765604315761045784350143751034561" ;

]

let util_tests = [
  str_encoding_test "Testing Long Number String Encoding" "8347201765604315761045784350143751034561" ;
  byte_encoding_test "Testing Long String Byte Encoding" "8347201765604315761045784350143751034561" ;
  packing_symmetry_test "List of ones packing test" [ Z.one ; Z.one ; Z.one ; Z.one ] 78 ;
]

let suite =
  "test suite for A2"
  >::: List.flatten [ ecc_tests; eddsa_tests; util_tests ]

let _ = run_test_tt_main suite
