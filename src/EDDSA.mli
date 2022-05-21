(** A collection of functions for signing and verifying messages *)

open Z

exception InvalidSignature
(** Raised when a signature is invalid *)

type signature
(** The signature of some message *)

val sign : Z.t -> Z.t -> signature
(** [sign m d] signs a message m with the private key d and returns
	the resulting signature *)

val verify : Z.t -> ED25519.point -> signature -> bool
(** [verify m p s] returns [true] if the signature [s] of message [m]
  generated using the private key associated with [p] *)

val digest_to_data : signature -> Z.t
(** [digest_to_data s] is data representation of s *)

val data_to_digest : Z.t -> signature
(** [data_to_digest n] is the signature representation of n *)

val string_of_digest : signature -> string
(** [digest_to_data s] is the string representation of s *)