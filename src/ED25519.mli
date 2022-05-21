(** Representation of the elliptic curve ed25519 *)

(** A point in a finite field *)
type point

val get_x_coord : point -> Z.t
(** [get_x_coord p] is the x-coordinate of p *)

val get_y_coord : point -> Z.t
(** [get_y_coord p] is the y-coordinate of p *)

val make_point : Z.t * Z.t -> point
(** [make_point (a,b)] makes a point with coordinates (a,b) *)

val raw_rep : point -> (Z.t * Z.t)
(** The raw representation of a point *)

val base : point
(** The base point *)

val ( + ) : point -> point -> point
(** The group operation of point arithmetic *)

val ( * ) : Z.t -> point -> point
(** The group operation repeated on a point *)

val string_of_point : point -> string
(** [string_of_point p] is the string representation of p *)

val order : Z.t
(** The order of the active curve *)

val equals : point -> point -> bool
(** [equals p1 p2] returns whether p1 and p2 are equal points *)
