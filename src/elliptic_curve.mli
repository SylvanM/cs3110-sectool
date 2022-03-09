(** Representation of an eliptic curve with arithmetic over the finite field *)

type field
(** The type of an elliptic curve along with its domain parameters *)

type point
(** A point in a finite field *)

exception InvalidPoint of point
(** Raised when a point is not in the correct field *)

val add_points : field -> point -> point -> point
(** [add_points] adds p1 and p2 in field f. Requires: [f] *)

val multiply_point : field -> Z.t -> point -> point
(** [multiply_point] adds a point with itself n times *)

val create_field : Z.t -> Z.t -> Z.t -> Z.t -> Z.t -> Z.t -> Z.t -> Z.t -> field
(** [create_point p a b c d g n h] creates an finite field for the elliptic 
curve defined by y^2 = ax^3 + bx^2 + cx + d *)