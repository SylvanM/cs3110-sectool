(** Representation of the elliptic curve ed25519 *)

(** A point in a finite field *)
type point

val get_x_coord : point -> Z.t

val make_point : Z.t * Z.t -> point

val base : point

val ( * ) : Z.t -> point -> point
(** The group operation of point arithmetic *)

val string_of_point : point -> string

(** Only temporarily visibile for testing *)

val slow_mul : Z.t -> point -> point

val ladder_mul : Z.t -> point -> point