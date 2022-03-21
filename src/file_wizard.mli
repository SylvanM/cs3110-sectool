(** A collection of functions helping with file I/O *)

exception FileDoesNotExist of string
(** Raised when a curve is loaded form a file but the file is invalid *)

val read_private_key : string -> Z.t
(** [read_private_key f] reads a file [f] and returns an integer which is the
    private key.
    Raises: [FileDoesNotExist] if [f] does not point to a valid file in /data/.
    Raises: [Malformed] if [f] is not formatted appropriately with spaces. *)

val read_public_key : string -> Elliptic_curve.point
(** [read_public_key f] reads a file [f] and returns a point which is the public
    key
    Raises: [FileDoesNotExist] if [f] does not point to a valid file in /data/.
    Raises: [Malformed] if [f] is not formatted appropriately with spaces. *)

val read_domain_params : string -> Elliptic_curve.field
(** [read_domain_params f] read reads a field, or domain parameters, from a
    file [f].
    Raises: [FileDoesNotExist] if [f] does not point to a valid file in /data/.
    Raises: [Malformed] if [f] is not formatted appropriately with spaces. *)

val write_private_key : Z.t -> string -> unit
(** [write_private_key d f] writes the key [d] to a file [f]. *)

val write_public_key : Elliptic_curve.point -> string -> unit
(** [write_public_key p f] writes the point [p] to a file [f]. *)

val write_domain_params : Elliptic_curve.field -> string -> unit
(** [write_domain_params dom f] writes information about field [dom] to file [f]. *)