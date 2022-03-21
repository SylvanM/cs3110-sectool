(** Representation of an eliptic curve with arithmetic over the finite field *)

type field
(** The type of an elliptic curve along with its domain parameters *)

type point
(** A point in a finite field *)

exception InvalidParameters
(** Raised when an invalid list of parameters is given for field construction *)

val add_points : field -> point -> point -> point
(** [add_points p1 p2 f] adds [p1] and [p2] in field [f] *)

val multiply_point : field -> Z.t -> point -> point
(** [multiply_point f n p] adds a point [p] with itself [n] times
    in field [f]. *)

val create_field : Z.t list -> field
(** [create_point [[p; a; b; c; d; g; n; h]]] creates an finite field for the
	elliptic curve defined by y^2 = ax^3 + bx^2 + cx + d *)

val deconstruct_field : field -> Z.t list
(** [deconstruct_field f] returns the list of integers used
to create the field [f]/ *)

val get_modulus : field -> Z.t
(** [get_modulus f] is the modulus of a field [f]. *)

val get_starting_point : field -> point
(** [get_starting_point f] is the starting point G of a field [f]. *)

val get_x_coord : point -> Z.t
(** [get_x_coord p] is x coordinate of point [p]. *)

val get_y_coord : point -> Z.t
(** [get_y_coord p] is y coordinate of point [p]. *)

val make_int_point : int * int -> point
(** [make_int_point c] is the [point] with coordinates specified by [c]. *)

val make_point : Z.t * Z.t -> point
(** [make_point c] is the [point] with coordinates specified by [c]. *)

val string_of_point : point -> string
(** [string_of_point p] is the string representation of point [p]. *)

val string_of_field : field -> string
(** [string_of_field f] is the string representation of point [f]. *)
