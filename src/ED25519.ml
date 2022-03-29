open Z

(* Represents an Affine point on an EC *)
type a_point = Point of { x : Z.t ; y : Z.t } | PointAtInfinty

(* Represents a proejctive point on an EC *)
type p_point = Point of { x : Z.t ; y : Z.t ; z : Z.t} | PointAtInfinty

type point = a_point

(* The type representing a montgomery curve *)
type m_curve = {
  modulus : Z.t ; (* Size of the field (prime in this case) *)
  a : Z.t ; (* coefficient a of the curve *)
  order : Z.t ;
  base_point : a_point ;
}

let ed25519 : m_curve = {
  modulus = (Z.pow (2 |> Z.of_int) (255)) - (19 |> Z.of_int) ; (* The size of the field *)
  a = 486662 |> Z.of_int ; (* The constant a *)
  order = (Z.pow (2 |> Z.of_int) (252) ) + ( "27742317777372353535851937790883648493" |> Z.of_string) ;
  base_point = Point {
    x = "15112221349535400772501151409588531511454012693041857206046113283949847762202" |> Z.of_string_base 10 ;
    y = "46316835694926478169428394003475163141307993866256225615783033603165251855960" |> Z.of_string_base 10 ;
  }
}

(* This is the definition of in infix mod operator that will always produce a positive integer *)
let rec ( % ) x y = 
	let m = x mod y in
		if m < Z.zero then 
			((x + y) % y) 
		else
			m

let add m a b =
  ((a % m) + (b % m)) % m

let sub m a b = 
  ((a % m) - (b % m)) % m

(** Define multiplication over a modulus m *)
let mul m a b =
  ((a % m) * (b % m)) % m

(* Computes the modular multiplicative inverse of a *)
let mul_inv m a =
  Z.invert a m

let div m a b =
  mul m a (mul_inv m b)

let pow m b p =
  Z.powm b (p |> Z.of_int) m

let (+) a b =
  add ed25519.modulus a b

let (-) a b = 
  sub ed25519.modulus a b 

let ( * ) a b =
  mul ed25519.modulus a b 

let ( / ) a b = 
  div ed25519.modulus a b 

let ( ** ) b p =
  pow ed25519.modulus b p

let a_to_p_point (ap : a_point) =
  match ap with 
  | Point { x = x ; y = y } -> Point { x = x ; y = y ; z = Z.one }
  | PointAtInfinty -> PointAtInfinty
  
let p_to_a_point (pp : p_point) : a_point =
  match pp with 
  | Point { x = x ; y = y ; z = z} -> Point { x = x / z ; y = y / z }
  | PointAtInfinty -> PointAtInfinty

(** General Arithmetic Functions *)

(** Computes the slope for the points *)
let lambda x1 y1 x2 y2 =
  if x1 = x2 then 
    let numerator = 
      let t1 =
        (Z.of_int 3) * (x1 ** 2) in 
      let t2 = 
        (Z.of_int 2) * ed25519.a * x1 in
      t1 + t2 + one in 
    let denominator = (Z.of_int 2) * y1 in 
    numerator / denominator
  else 
    let numerator = y2 - y1 in 
    let denominator = x2 - x1 in 
    numerator / denominator

(** Computes the result of the operation of adding the two point values *)
let a_compute_next x1 y1 x2 y2 =
  let l = lambda x1 y1 x2 y2 in 
  let x = (l ** 2) - ed25519.a - x1 - x2 in 
  let y = l * (x1 - x) - y1 in 
  (x, y)
    
let a_double (p : a_point) : a_point =
  match p with
  | PointAtInfinty -> PointAtInfinty
  | Point p ->
    if p.y = Z.zero then PointAtInfinty else 
    let (x, y) = a_compute_next p.x p.y p.x p.y in 
    Point {
      x = x ;
      y = y
    }

let a_add (p : a_point) (q : a_point) : a_point =
  if p = q then a_double p else
  match p with 
  | PointAtInfinty -> q
  | Point p -> 
    match q with 
    | PointAtInfinty -> Point p
    | Point q ->
      if p.x = q.x then PointAtInfinty else
        let (x, y) = a_compute_next p.x p.y q.x q.y in 
        Point {
          x = x ;
          y = y ;
        }

(* Doubles a point p n times, aka multiplies p by 2^n *)
let a_power_of_two p n =
  let rec a_pt_tail k acc =
    if k = 0 then acc else
      a_pt_tail (Int.sub k 1) (a_double acc) in 

  a_pt_tail n p

let ( + ) (p : a_point) (q : a_point) = 
  a_add p q

let a_mul k p = 
  let rec a_mul_tail (k : Z.t) (place : int) (acc : a_point) : a_point =
    if k = zero then acc else 
      let new_k = shift_right k 1 in 
      let new_place = Int.add place 1 in 
      let new_acc = 
        if is_odd k then
          a_add acc (a_power_of_two p place)
        else acc in 
      a_mul_tail new_k new_place new_acc 
    in
  
  a_mul_tail k 0 PointAtInfinty

let ( * ) (k : Z.t) (p : a_point) =
  a_mul k p

let base = ed25519.base_point

let p_add (p : p_point) (q : p_point) =
  raise (Failure "Unimplemented")

let p_mul (k : Z.t) (p : p_point) =
  raise (Failure "Unimplemented")

let get_x_coord (p : a_point) = 
  match p with 
  | PointAtInfinty -> Z.minus_one
  | Point po -> po.x

let get_y_coord (p : a_point) = 
  match p with 
  | PointAtInfinty -> Z.minus_one
  | Point po -> po.y

(** EXPOSED **)

(* let better_multiply_point f n p =
  let rec tail_mul n place acc =
    if n = Z.zero then acc else
    if Z.is_odd n then tail_mul (Z.shift_right n 1) (Int.add place 1) 
      (add_points f acc (power_of_two f p place))
    else 
      tail_mul (Z.shift_right n 1) (Int.add place 1) (acc)
    in
  tail_mul n 0 PointAtInfinty *)

(* let multiply_point = better_multiply_point *)

let make_point (p : Z.t * Z.t) : a_point =
  match p with 
  | (x, y) -> Point {x = x ; y = y}

let make_int_point (p : int * int) = 
  match p with 
  | (x, y) -> make_point (Z.of_int x, Z.of_int y)

let string_of_point (p : a_point) = 
  match p with 
  | PointAtInfinty -> "Inf"
  | Point p -> (p.x |> Z.to_string) ^ " " ^ (p.y |> Z.to_string)