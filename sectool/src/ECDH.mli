open ED25519

(** Module for the actual algorithms of the Elliptic Curve Diffie-Helman key
exchange *)

val generate_private_key : int -> Z.t
(** [generate_private_key n] is a random integer for use as a private key
	with maximum value [n]. *)

val compute_public_key : Z.t -> point
(** [compute_public_key f d] computes the public key from a private key d *)

val compute_shared_secret : Z.t -> point -> point
(** [compute_shared_secret f d p] uses this party's private key and the other 
	party's pubic key p to generate shared secret *)
