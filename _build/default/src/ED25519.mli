(** Representation of the elliptic curve ed25519 *)

(** A point in a finite field *)
type point

(** [p + q] adds p and pq in field ed25519. *)
val ( + ) : point -> point -> point

(** [k * p] *)
val ( * ) : Z.t -> point -> point

val base : point

val get_x_coord : point -> Z.t

val make_int_point : int * int -> point
val make_point : Z.t * Z.t -> point

val string_of_point : point -> string
