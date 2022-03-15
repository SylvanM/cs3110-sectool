(** Representation of an eliptic curve with arithmetic over the finite field *)

(** The type of an elliptic curve along with its domain parameters *)
type field

(** A point in a finite field *)
type point

(** Raised when a point is not in the correct field *)
exception InvalidPoint of point

(** [add_points] adds p1 and p2 in field f. Requires: [f] *)
val add_points : field -> point -> point -> point

(** [multiply_point] adds a point with itself n times *)
val multiply_point : field -> Z.t -> point -> point

(** [create_point (p, a, b, c, d, g, n, h)] creates an finite field for the elliptic
curve defined by y^2 = ax^3 + bx^2 + cx + d *)
val create_field : Z.t * Z.t * Z.t * Z.t * Z.t * Z.t * Z.t * Z.t -> field

(** [deconstruct_field f] returns the tuple of integers used
to create the field f *)
val deconstruct_field : field -> Z.t * Z.t * Z.t * Z.t * Z.t * Z.t * Z.t * Z.t
