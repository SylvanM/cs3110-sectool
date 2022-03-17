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

exception InvalidPoint of point
exception InvalidParameters

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
  let lambda = ((3 |> Z.of_int) * (Z.pow p.x 2) + f.c) 
    / ((2 |> Z.of_int) * p.y) in 
  lambda_intersection lambda p p (* TODO - Maybe make this more efficient? *)

(* Doubles a point p on f n times *)
let power_of_two f p n =
  let rec tail_pow_two (k : int) (acc : point) : point = 
    if k = 0 then acc else 
      let thing = ((k |> Z.of_int) - Z.one) |> Z.to_int in 
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
  (* raise (Failure "Unimplemented: add_points" *)
  simple_add p1 p2

let multiply_point f n p = 
  let rec tail_multiply (n : Z.t) (acc : point) (place : Z.t) : point = 
    if n = Z.zero then acc else
    if n = Z.one then p else if n = (2 |> Z.of_int) then double f p else
      if Z.is_odd n then tail_multiply (Z.shift_left n 1) (add_points f acc p) 
        (place + Z.one) else 
        tail_multiply (Z.shift_left n 1) (acc) (place +  Z.one)
    in
    
  tail_multiply n p Z.zero

let get_point_at f x = 
  { y = (f.a * (Z.pow x 3)) + (f.b * (Z.pow x 2)) + (f.c * x) + f.d |> Z.sqrt ;
  x = x }

let create_field (parameters : Z.t list) : field =
  match parameters with 
  | [p ; a ; b ; c ; d ; gx ; n ; h] ->
    let temp_f = { 
      p = p; 
      a = a; 
      b = b; 
      c = c;
      d = d; 
      g = 
      {
        x = gx ;
        y = Z.zero ;
      }; 
      n = n; 
      h = h; 
    } 

    in 

    { temp_f with g = get_point_at temp_f temp_f.g.x }
    
  | _ -> raise InvalidParameters

let deconstruct_field f =
  [f.p ; f.a ; f.b ; f.c ; f.d ; f.g.x ; f.n ; f.h]

let get_modulus f = f.p

let get_starting_point f = f.g

let get_x_coord p = p.x

