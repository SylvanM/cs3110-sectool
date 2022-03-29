(** A collection of functions helping with file I/O *)

exception FileDoesNotExist of string
(** Raised when a curve is loaded form a file but the file is invalid *)

val read_private_key : string -> Z.t
  (** [read_private_key f] reads a file and returns an integer which is the
    private key *)

val read_public_key : string -> ED25519.point
  (** [read_public_key f] reads a file and returns a point which is the public
    key *)

val write_private_key : Z.t -> string -> unit
  (** [write_private_key d f] writes the key d to a file f *)