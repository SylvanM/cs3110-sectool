(** Representation of an eliptic curve with arithmetic over the finite field *)

(** The type of an elliptic curve along with its domain parameters *)
type field

(** A point in a finite field *)
type point

exception InvalidParameters
(** Raised when an invalid list of parameters is given for field construction *)

(** [add_points] adds p1 and p2 in field f. Requires: [f] *)
val add_points : field -> point -> point -> point

(** [multiply_point] adds a point with itself n times *)
val multiply_point : field -> Z.t -> point -> point

val create_field : Z.t list -> field
(** [create_point [[p; a; b; c; d; g; n; h]]] creates an finite field for the 
	elliptic curve defined by y^2 = ax^3 + bx^2 + cx + d *)

val deconstruct_field : field -> Z.t list
(** [deconstruct_field f] returns the list of integers used 
to create the field f *)

val get_modulus : field -> Z.t
(** Returns the modulus of a field *)

val get_starting_point : field -> point
(** Returns the starting point G of a field *)

val get_x_coord : point -> Z.t
val get_y_coord : point -> Z.t

val make_int_point : int * int -> point
val make_point : Z.t * Z.t -> point

val string_of_point : point -> string
val string_of_field : field -> string
