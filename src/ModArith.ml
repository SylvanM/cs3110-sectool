(** A module for modular arithmetic *)

open Z

let rec ( % ) x y =
  let m = x mod y in
    if m < zero then
      ((x + y) % y)
    else
      m

module type ModulusContainer = sig
  val modulus : Z.t
end

module type ModOps = sig

  val ( + ) : Z.t -> Z.t -> Z.t 

  val ( - ) : Z.t -> Z.t -> Z.t 

  val ( * ) : Z.t -> Z.t -> Z.t 

  val ( / ) : Z.t -> Z.t -> Z.t 

  val ( ** ) : Z.t -> int -> Z.t

end

module Make (M : ModulusContainer) : ModOps = struct

  let add m a b =
    ((a % m) + (b % m)) % m
  
  let sub m a b =
    ((a % m) - (b % m)) % m
  
  (** Define multiplication over a modulus m *)
  let mul m a b =
    ((a % m) * (b % m)) % m
  
  let pow m b p =
    Z.powm b (p |> Z.of_int) m
  
  let mul_inv m a =
    Z.invert a m
  
  let div m a b =
    mul m a (mul_inv m b)
  
  
  let ( + ) =
    add M.modulus
  
  let ( - ) =
    sub M.modulus 
  
  let ( * )  =
    mul M.modulus 
  
  let ( / )  =
    div M.modulus 
  
  let ( ** )  =
    pow M.modulus 

end
