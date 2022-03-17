(** Module for the actual algorithms of the Elliptic Curve Diffie-Helman key 
exchange *)

val generate_private_key : Elliptic_curve.field -> Z.t 
(** Generates a random integer for use as a private key *)

val compute_public_key : Elliptic_curve.field -> Z.t -> Z.t
(** [compute_public_key f d] computes the public key from a private key d *)

val compute_shared_secret : Elliptic_curve.field -> Z.t -> Z.t -> Z.t
(** [compute_shared_secret f d p] uses this party's private key and the other 
	party's pubic key p to generate shared secret *)