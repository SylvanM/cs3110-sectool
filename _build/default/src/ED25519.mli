(** Representation of the elliptic curve ed25519 *)

(** A point in a finite field *)
type point

val get_x_coord : point -> Z.t

val make_point : Z.t * Z.t -> point

val raw_rep : point -> (Z.t * Z.t)
(** The raw representation of a point *)

val base : point

val ( + ) : point -> point -> point
(** The group operation of point arithmetic *)

val ( * ) : Z.t -> point -> point
(** The group operation repeated on a point *)

val string_of_point : point -> string

val order : Z.t

val equals : point -> point -> bool
