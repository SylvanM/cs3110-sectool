(** Representation of an eliptic curve with arithmetic over the finite field *)

(** The type of an elliptic curve along with its domain parameters *)
type field = {
  p : int ; (* The size of the field *)
  a : int ; (* The constant a *)
  b : int ; (* The constant b *)
  c : int ; (* The curve constant c *)
  d : int ; (* The curve constant d *)
  g : int ; (* The starting point *)
  n : int ; (* The order of G *)
  h : int ; (* The co-factor *)
}

(** A point in a finite field *)
type point = {
  x : int ;
  y : int ;
}

(** Raised when a point is not in the correct field *)
exception InvalidPoint of point

(** [add_points] adds p1 and p2 in field f. Requires: [f] *)
val add_points : field -> point -> point -> point

(** [multiply_point] adds a point with itself n times *)
val multiply_point : field -> int -> point -> point

(** [create_point (p, a, b, c, d, g, n, h)] creates an finite field for the elliptic
curve defined by y^2 = ax^3 + bx^2 + cx + d *)
val create_field : int * int * int * int * int * int * int * int -> field

(** [deconstruct_field f] is the tuple of integers used
to create the field f *)
val deconstruct_field : field -> int * int * int * int * int * int * int * int

(** [string_of_point p] is the string of the parameters of [p]
separated by single spaces *)
val string_of_point : point -> string

(** [string_of_field f] is the string of the parameters of [f]
separated by single spaces *)
val string_of_field : field -> string

