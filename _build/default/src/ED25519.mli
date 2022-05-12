(** Representation of the elliptic curve ed25519 *)

(** A point in a finite field *)
type point

val get_x_coord : point -> Z.t

val make_point : Z.t * Z.t -> point

val string_of_point : point -> string