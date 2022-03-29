open ED25519

let generate_private_key bit_size =
  let rec construct_random_integer (i : int) (bits_left : int) (acc : Z.t) =
    if bits_left <= 0 then acc else 
      construct_random_integer (Int.add i 1) (bits_left - 30) 
      ( Z.add acc ((Z.shift_left (Random.bits () |> Z.of_int) (Int.mul i 30))) )
    in

  bit_size |> Z.of_int |> Z.(mod) (construct_random_integer 0 bit_size Z.zero)

let compute_public_key (d : Z.t) =
  d * base

let compute_shared_secret d p =
  d * p
