open Z

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

exception InvalidPoint of point

(** EXPOSED **)

let add_points f p1 p2 =
  raise (Failure "Unimplemented: from_file")
(** [add_points] adds p1 and p2 in field f. Requires: [f] *)

let multiply_point f n p = 
  raise (Failure "Unimplemented: from_file")
(** [multiply_point] adds a point with itself n times *)


(** HIDDEN **)

let lambda_intersection l p1 p2 = 
  let x = (Z.pow l 2) - p1.x - p2.x in 
  let y = l * (p1.x - x) - p1.y in 
  { x = x ; y = y }

(* Adds two points using standard calculation, no tricks used *)
let simple_add p1 p2 = 
  let lambda = (p2.y - p1.y) / (p2.x - p2.x) in 
  lambda_intersection lambda p1 p2

(* Doubles a point by adding it to itself, 
  using the tangent line to the curve *)
let double f p =
  let lambda = ((3 |> Z.of_int) * (Z.pow p.x 2) + f.a) 
    / ((2 |> Z.of_int) * p.y) in 
  lambda_intersection lambda p p (* TODO - Maybe make this more efficient? *)

(* Negates a point *)
let negate p = 
  { p with y = -p.y }