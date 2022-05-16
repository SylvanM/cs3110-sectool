(** A collection of functions for signing and verifying messages *)

open Z

exception InvalidSignature

type signature 
(** The signature of some message *)

val sign : Z.t -> Z.t -> signature
(** [sign m d] signs a message m with the private key d and returns 
	the resulting signature *)

val verify : Z.t -> ED25519.point -> signature -> bool
(** [verify m p s] returns [true] if the signature [s] of message [m]
  generated using the private key associated with [p] *)

val digest_to_data : signature -> Z.t 

val data_to_digest : Z.t -> signature