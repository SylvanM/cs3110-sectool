open Z 
open ED25519
open ECDH
open ModArith

exception InvalidSignature

type signature = (Z.t * Z.t)

let combine = EncodingUtility.pack 256 

let decombine = EncodingUtility.unpack 256

let concatenate_integers = 
  let rec concat_tail acc = function 
  | [] -> acc 
  | number :: t ->
    let bit_width = 256 in 
    let shifted_acc = Z.shift_left acc bit_width in 
    let new_acc = Z.add shifted_acc number in 
    concat_tail new_acc t 
  in 
  concat_tail zero

let hash k = 
  k |> to_string |> Sha256.string |> Sha256.to_hex |> Z.of_string_base 16


let sign message privkey : signature =
  let open ModArith.Make (struct let modulus = ED25519.order end) in 
  let h = hash message in 
  let k = generate_private_key () % ED25519.order in 
  let big_r = compute_public_key k in 
  let r = big_r |> get_x_coord in 
  let s = (h + r * privkey) / k in 
  (r, s)



let verify message pubkey (r, s) = 
  let module M = ModArith.Make (struct let modulus = ED25519.order end) in
  let h = hash message in
  let s' = M.inv s in 
  let big_r' = ED25519.( + ) (compute_public_key (Z.mul s' h)) (ED25519.( * ) (Z.mul s' r) pubkey) in 
  let r' = big_r' |> get_x_coord in 
  r' = r

let string_of_digest = function 
  | (r, s) -> (Z.to_string r) ^ " " ^ (Z.to_string s)


let digest_to_data (r, s) = [r ; s] |> combine

let data_to_digest data : signature = 
  match decombine 2 data with 
  | [r ; s] -> (r, s) 
  | _ -> raise InvalidSignature