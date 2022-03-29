open ED25519

(** Module for the actual algorithms of the Elliptic Curve Diffie-Helman key 
exchange *)

val generate_private_key : int -> Z.t 
(** Generates a random integer for use as a private key, with a certain bit size *)

val compute_public_key : Z.t -> point
(** [compute_public_key f d] computes the public key from a private key d *)

val compute_shared_secret : Z.t -> point -> point
(** [compute_shared_secret f d p] uses this party's private key and the other 
	party's pubic key p to generate shared secret *)