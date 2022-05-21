(** A module for modular arithmetic *)

open Z

val ( % ) : Z.t -> Z.t -> Z.t
(** Modulus for Z.t integers *)

module type ModulusContainer = sig
  val modulus : Z.t
  (** Modulus operator for Z.t integers *)
end

module type ModOps = sig

  val ( + ) : Z.t -> Z.t -> Z.t
  (** Addition for Z.t integers *)

  val ( - ) : Z.t -> Z.t -> Z.t
  (** Subtraction for Z.t integers *)

  val ( * ) : Z.t -> Z.t -> Z.t
  (** Multiplication for Z.t integers *)

  val ( / ) : Z.t -> Z.t -> Z.t
  (** Division for Z.t integers *)

  val ( ** ) : Z.t -> int -> Z.t
  (** Exponentiation for Z.t integers *)

  val inv : Z.t -> Z.t
  (** Inversion for Z.t integers *)

end

module Make : functor (M : ModulusContainer) -> ModOps