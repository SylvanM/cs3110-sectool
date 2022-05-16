open Z
open ModArith

let pack width =
  let rec tail_combine acc index = function 
    | [] -> acc 
    | v :: t -> 
      let new_to_add = Z.shift_left v (Int.mul width index) in 
      let new_acc = Z.add acc new_to_add in 
      let new_index = Int.add index 1 in 
      tail_combine new_acc new_index t
    in 
  tail_combine zero 0

let str_to_bytes s =
  s |> String.to_seq |> List.of_seq |> List.map Char.code

let bytes_to_str b = 
  b |> List.map Char.chr |> List.to_seq |> String.of_seq

let concat_bits bits len data = 
  (Z.shift_left data len) + (bits % (Z.shift_left one len))
    
let rec unpack width len data = 
  if len = 0 then [] else 
  let this_chunk = data % (Z.shift_left one width) in 
  let new_data = Z.shift_right_trunc data width in 
  this_chunk :: unpack width (Int.sub len 1) new_data

let encode_string s =
  s |> str_to_bytes |> List.map Z.of_int |> pack 8

let decode_string d = 
  let rec decode_tail acc data =
    if data = zero then acc else 
      let these_bits = data % (Z.shift_left one 8) in 
      let new_data = Z.shift_right_trunc data 8 in 
      let this_char = these_bits |> Z.to_int |> Char.chr in 
      let new_acc = acc ^ String.make 1 this_char in 
      decode_tail new_acc new_data
    in
  decode_tail "" d
  