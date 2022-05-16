(** A module for modular arithmetic *)

open Z

val ( % ) : Z.t -> Z.t -> Z.t

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

module Make : functor (M : ModulusContainer) -> ModOps