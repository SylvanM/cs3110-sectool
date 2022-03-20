open Elliptic_curve

let generate_private_key f =
  let bit_size = f |> get_modulus |> Z.log2 |> (+) 1 in

  let rec construct_random_integer (i : int) (bits_left : int) (acc : Z.t) =
    if bits_left <= 0 then acc else 
      construct_random_integer (i + 1) (bits_left - 30) 
      ( Z.add acc ((Z.shift_left (Random.bits () |> Z.of_int) (i * 30))) )
    in

  bit_size |> Z.of_int |> Z.(mod) (construct_random_integer 0 bit_size Z.zero)

let compute_public_key (f : field) (d : Z.t) =
  (f |> get_starting_point |> multiply_point f d |> get_x_coord)

let compute_shared_secret f d p =
  (* raise (Failure "Unimplemented") *)
  (p |> multiply_point f d |> get_x_coord)
