open Z 
open ED25519
open ModArith

exception InvalidSignature

type signature = (point * Z.t)

let combine =
  let rec tail_combine acc index = function 
    | [] -> acc 
    | v :: t -> 
      let new_to_add = Z.shift_left v (Int.mul 256 index) in 
      let new_acc = Z.add acc new_to_add in 
      let new_index = Int.add index 1 in 
      tail_combine new_acc new_index t
    in 
  tail_combine zero 0
    
let rec unpack data len = 
  if len = 0 then [] else 
  let this_chunk = data % (Z.of_int 256) in 
  let new_data = Z.shift_right_trunc data 256 in 
  this_chunk :: unpack new_data (Int.sub len 1)

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
  match unpack data 3 with 
  | [a ; b ; s] -> 
    let big_r = make_point (a, b) in 
    (big_r, s) 
  | _ -> raise InvalidSignature