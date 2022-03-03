
type field = {
  p : Z.t ; (* The size of the field *)
  a : Z.t ; (* The constant a *)
  b : Z.t ; (* The constant b *)
  g : Z.t ; (* The starting point *)
  n : Z.t ; (* The order of G *)
  h : Z.t ; (* The co-factor *)
}

type point = {
  x : Z.t ;
  y : Z.t ;
}

exception InvalidCurveFile of string 
exception InvalidPoint of point

let from_file f =
  raise (Failure "Unimplemented: from_file")
(** [from_file f] is the field that [f] represents. Requires: [f] is
    a valid JSON field representation. *)

let add_points f p1 p2 =
  raise (Failure "Unimplemented: from_file")
(** [add_points] adds p1 and p2 in field f. Requires: [f] *)

let multiply_point f n p = 
  raise (Failure "Unimplemented: from_file")
(** [multiply_point] adds a point with itself n times *)