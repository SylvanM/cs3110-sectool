open ED25519

let generate_private_key bit_size =
  let rec construct_random_integer (i : int) (bits_left : int) (acc : Z.t) =
    if bits_left <= 0 then acc else
      construct_random_integer (Int.add i 1) (bits_left - 30)
      ( Z.add acc ((Z.shift_left (Random.bits () |> Z.of_int) (Int.mul i 30))) )
    in

  construct_random_integer 0 bit_size Z.zero

let compute_public_key (d : Z.t) =
  d * base (* |> get_x_coord *)

let compute_shared_secret d p =
  (* let p_as_point = make_point (p, Z.one) in 
  d * p_as_point |> get_x_coord *)
  d * p