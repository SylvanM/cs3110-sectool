open Z 
open ED25519
open ModArith

exception InvalidSignature

type signature = (point * Z.t)

let combine = EncodingUtility.pack 256 

let decombine = EncodingUtility.unpack 256

let hash k = 
  k |> to_string |> Sha512.string |> Sha512.to_hex |> Z.of_string_base 16


let sign message privkey : signature =
  let pubkey = privkey * base in
  let r = 
    let inner = [ hash privkey ; message ] |> combine in 
    hash inner % order in 
  let big_r = r * base in 
  let h = 
    let to_be_hashed = 
      [ get_x_coord big_r ; get_x_coord pubkey ; message ] |> combine in
    hash to_be_hashed % order in 
  let s = (Z.add r (Z.mul h privkey)) % order in 
  (big_r, s)

let verify message pubkey (big_r, s) = 
  let h = 
    let to_be_hashed = 
      [ get_x_coord big_r ; get_x_coord pubkey ; message ] |> combine in
    hash to_be_hashed % order in 
  let p1 = s * base in 
  let p2 = big_r + (h * pubkey) in 
  equals p1 p2

let digest_to_data (big_r, s) = 
  let raw_point = match raw_rep big_r with
    | (a, b) -> [a ; b] in 
  raw_point @ [s] |> combine

let data_to_digest data : signature = 
  match decombine 3 data with 
  | [a ; b ; s] -> 
    let big_r = make_point (a, b) in 
    (big_r, s) 
  | _ -> raise InvalidSignature