(** Representation of an eliptic curve with arithmetic over the finite field *)

type field
(** The type of an elliptic curve along with its domain parameters *)

type point
(** A point in a finite field *)

exception InvalidCurveFile of string
(** Raised when a curve is loaded form a file but the file is invalid *)

exception InvalidPoint of point
(** Raised when a point is not in the correct field *)

val from_file : string -> field
(** [from_file f] is the field that [f] represents. Requires: [f] is
    a valid JSON field representation. *)

val add_points : field -> point -> point -> point
(** [add_points] adds p1 and p2 in field f. Requires: [f] *)

val multiply_point : field -> Z.t -> point -> point
(** [multiply_point] adds a point with itself n times *)