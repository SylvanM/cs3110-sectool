open OUnit2
open Sectool
open ED25519
open Testing_constants
open ECDH
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
  name >:: fun _ -> assert_equal s1 s2 ~printer:Z.to_string

let ladder_same_test name k =
  name >:: fun _ ->
    assert_equal (slow_mul k base) (ladder_mul k base) 
      ~printer:string_of_point

let communativity_test name dA dB = 
  name >:: fun _ -> 
    assert_equal (dA * (dB * base)) (dB * (dA * base)) 
      ~printer:string_of_point

let associativity_test name a b =
  name >:: fun _ ->
    assert_equal 
      (a * (b * base)) ((Z.mul a b) * base)
      ~printer:string_of_point

let slow_mul_commutes name dA dB = 
  name >:: fun _ ->
    assert_equal 
    (slow_mul dA (slow_mul dB base) |> get_x_coord) 
    (slow_mul dB (slow_mul dA base) |> get_x_coord) ~printer:Z.to_string

let rec communativity_tests count bit_size =
  if count = 0 then [] else
    let dA = generate_private_key bit_size in 
    let dB = generate_private_key bit_size in 
    let test = communativity_test ("Commun Test " ^ string_of_int count)
      dA dB in 
    test :: (communativity_tests (count - 1) bit_size)

let rec square_assoc_tests count =
  if count < 0 then [] else 
    let test = associativity_test 
      ("Sq. Assoc Test: " ^ string_of_int count) 
      (Z.of_int count) (Z.of_int count) in 
    test :: (square_assoc_tests (count - 1))

let rec associativity_tests count bit_size =
  if count = 0 then [] else
    let dA = generate_private_key bit_size in 
    let dB = generate_private_key bit_size in 
    let test = communativity_test ("Assoc Test " ^ string_of_int count)
      dA dB in 
    test :: (communativity_tests (count - 1) bit_size)



let rec ladder_tests count = 
  if count < 0 then [] else 
    let test = ladder_same_test 
      ("Ladder Func Test: " ^ string_of_int count)
      (Z.of_int count) in 
    test :: (ladder_tests (count - 1))

    


(* let pub_key_gen_test name d_str expected_pub_str =
  name >:: fun _ -> 
    let d = Z.of_string_base 16 (d_str |> decode_ucoord)  in 
    let expected_pub = Z.of_string_base 16 (expected_pub_str |> decode_ucoord) in
    let computed_pub = compute_public_key d in 
    assert_equal expected_pub computed_pub ~printer:Z.to_string *)
  
(** TESTS  *)

let ecc_tests = [

  same_secret_test "Small Key Test" (4 |> Z.of_int) (7 |> Z.of_int) ;

  (* same_secret_test "Basic Key test" (234 |> Z.of_int) (75463 |> Z.of_int) ; *)
  (* same_secret_test "Larger Key Test" ("8347201765604315761045784350143751034561" |> Z.of_string) ("4534825034158085927349874325" |> Z.of_string) ; *)

  (* pub_key_gen_test "Foo"
    "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"
    "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a" ; *)

  slow_mul_commutes "1 commutes with 2" Z.one (Z.of_int 2) ;
  slow_mul_commutes "2 commutes with 3" (Z.of_int 2) (Z.of_int 3) ;
  slow_mul_commutes "3 commutes with 4" (Z.of_int 3) (Z.of_int 4) ;
  slow_mul_commutes "4 commutes with 5" (Z.of_int 4) (Z.of_int 5) ;

] 
(* @ ladder_tests 100 *)
(* @ communativity_tests 10 4 *)
(* @ associativity_tests 1 1 *)
(* @ square_assoc_tests 5 *)

let aes_tests = [

]

let suite =
  "test suite for A2"
  >::: List.flatten [ ecc_tests; aes_tests; ]

let _ = run_test_tt_main suite
