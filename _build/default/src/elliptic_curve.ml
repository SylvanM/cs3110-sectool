open Z

(* 
An elliptic curve is a curve of the form
y^2 = ax^3 + bx^2 + cx + d
*)

type point = {
  x : Z.t ;
  y : Z.t ;
}

type field = {
  p : Z.t ; (* The size of the field *)
  a : Z.t ; (* The constant a *)
  b : Z.t ; (* The constant b *)
  c : Z.t ; (* The curve constant c *)
  d : Z.t ; (* The curve constant d *)
  g : point ; (* The starting point *)
  n : Z.t ; (* The order of G *)
  h : Z.t ; (* The co-factor *)
}

exception InvalidParameters

(* This is the definition of in infix mod operator that will always produce a positive integer *)
let rec ( % ) x y = 
	let m = x mod y in
		if m < Z.zero then 
			((x + y) % y) 
		else
			m

(** HIDDEN **)

(* Adds two points using standard calculation, no tricks used *)
let simple_add f p1 p2 = 
  let lambda = (p2.y - p1.y) / (p2.x - p1.x) in 
  let x3 = (((Z.pow lambda 2)) - p1.x - p2.x) % f.p in 
  let y3 = (lambda * (p1.x - x3) - p1.y) % f.p in 
  {x = x3; y = y3}

(* Doubles a point by adding it to itself, 
  using the tangent line to the curve *)
let double f p =
  if p.y = Z.zero then p else
  let lambda = ( ( (3 |> Z.of_int) * f.a * (Z.pow p.x 2) ) + ((2 |> Z.of_int) * f.b * p.x) + f.c ) / ((2 |> Z.of_int) * p.y) in 
  let x2 = (Z.pow lambda 2) - (2 |> Z.of_int) * p.x in 
  let y2 = lambda * (p.x - x2) - p.y in 
  {x = x2 ; y = y2}

(* Doubles a point p on f n times *)
let power_of_two f p n =
  let rec tail_pow_two (k : int) (acc : point) : point = 
    if k = 0 then acc else 
      let thing = ((k |> Z.of_int) - Z.one) |> Z.to_int in 
      (* I know this looks super wacky, but I couldn't get it to compile
      otherwise because of how OCaml was doing its type inference *)
      tail_pow_two thing (double f acc)
    in
  tail_pow_two n p

(** Computes all points on the curve *)
let points_on f =
  let rec tail_points_find (points : point list) (x : Z.t) : (point list * Z.t) =
    if x >= f.p then (points, x) else
      let w = ((f.a * (Z.pow x 3)) + (f.b * (Z.pow x 2)) + (f.c * x) + f.d ) % f.p in 
      (** Check if w is perfect square *)
      if Z.perfect_square w then 
        let y = w |> Z.sqrt in 
          tail_points_find ( { x = x ; y = y } :: { x = x ; y = -y } :: points ) (x + Z.one)
      else tail_points_find points (x + Z.one) 
    in
  tail_points_find [] Z.zero

(* Negates a point *)
let negate p = 
  { p with y = -p.y }

let inverse p = { p with y = -p.y }

let get_x_coord p = p.x
let get_y_coord p = p.y

(** EXPOSED **)

let add_points f p1 p2 =
  if p1 = p2 then double f p1 else
    simple_add f p1 p2
  
let multiply_point f n p = 
  let rec tail_multiply (n : Z.t) (acc : point) (place : Z.t) : point = 
    if n = Z.zero then acc else
    if n = Z.one then p else if n = (2 |> Z.of_int) then double f p else
      if Z.is_odd n then tail_multiply (Z.shift_left n 1) (add_points f acc p) 
        (place + Z.one) else 
        tail_multiply (Z.shift_left n 1) (acc) (place +  Z.one)
    in
    
  tail_multiply n p Z.zero

let create_field (parameters : Z.t list) : field =
  match parameters with 
  | [p ; a ; b ; c ; d ; gx ; gy ; n ; h] ->
    { 
      p = p; 
      a = a; 
      b = b; 
      c = c;
      d = d; 
      g = 
      {
        x = gx ;
        y = gy;
      }; 
      n = n; 
      h = h; 
    } 
    
  | _ -> raise InvalidParameters

let deconstruct_field f =
  [f.p ; f.a ; f.b ; f.c ; f.d ; f.g.x ; f.g.y ; f.n ; f.h]

let get_modulus f = f.p

let get_starting_point f = f.g

let make_point (p : Z.t * Z.t) =
  match p with 
  | (x, y) -> {x = x ; y = y}

let make_int_point (p : int * int) = 
  match p with 
  | (x, y) -> make_point (Z.of_int x, Z.of_int y)

let string_of_point p = (p.x |> Z.to_string) ^ " " ^ (p.y |> Z.to_string)

let string_of_field f =
  let rec construct_str params =
    match params with
    | [] -> ""
    | h :: t -> (h |> Z.to_string) ^ (construct_str t)
  in 
  f |> deconstruct_field |> construct_str