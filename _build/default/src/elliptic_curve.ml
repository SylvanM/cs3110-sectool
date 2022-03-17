let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

(*
An elliptic curve is a curve of the form
y^2 = ax^3 + bx^2 + cx + d
*)
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

type point = {
  x : int ;
  y : int ;
}

exception InvalidPoint of point

(** HIDDEN **)

let lambda_intersection l p1 p2 =
  let x = (pow l 2) - p1.x - p2.x in
  let y = l * (p1.x - x) - p1.y in
  { x = x ; y = y }

(* Adds two points using standard calculation, no tricks used *)
let simple_add p1 p2 =
  let lambda = (p2.y - p1.y) / (p2.x - p2.x) in
  lambda_intersection lambda p1 p2

(* Doubles a point by adding it to itself,
  using the tangent line to the curve *)
let double f p =
  let lambda = ((3) * (pow p.x 2) + f.c)
    / ((2) * p.y) in
  lambda_intersection lambda p p (* TODO - Maybe make this more efficient? *)

(* Doubles a point p on f n times *)
let power_of_two f p n =
  let rec tail_pow_two (k : int) (acc : point) : point =
    if k = 0 then acc else
      let thing = k-1 in
      (* I know this looks super wacky, but I couldn't get it to compile
      otherwise... *)
      tail_pow_two thing (double f acc)
    in
  tail_pow_two n p

(* Negates a point *)
let negate p =
  { p with y = -p.y }

(** EXPOSED **)

let add_points f p1 p2 =
  raise (Failure "Unimplemented: add_points")

let multiply_point f n p =
  let rec tail_multiply (n : int) (acc : point) (place : int) : point =
    if n = 0 then acc else
    if n = 1 then p else if n = 2 then double f p else
      if (n mod 2 = 1) then tail_multiply (Int.shift_left n 1) (add_points f acc p) (place + 1) else
        tail_multiply (Int.shift_left n 1) (acc) (place +  1)
    in
  tail_multiply n p 0


let create_field parameters : field =
  match parameters with
  | (p, a, b, c, d, g, n, h) ->
  {
    p = p; a = a; b = b; c = c; d = d; g = g; n = n; h = h;
  }

let deconstruct_field f =
  (f.p, f.a, f.b, f.c, f.d, f.g, f.n, f.h)

let string_of_point p =
  (string_of_int p.x) ^ " " ^ (string_of_int p.y)

let string_of_field f =
  let (p,a,b,c,d,g,n,h) = deconstruct_field f in
    let list = [p;a;b;c;d;g;n;h] in
      let rec to_string l =
        match l with
        | h::t -> (string_of_int h) ^ " " ^ (to_string t)
        | [] -> ""
      in to_string list