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

let ed25510 : m_curve = {
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
  add ed25510.modulus a b

let (-) a b = 
  sub ed25510.modulus a b 

let ( * ) a b =
  mul ed25510.modulus a b 

let ( / ) a b = 
  div ed25510.modulus a b 

let ( ** ) b p =
  pow ed25510.modulus b p

let a_to_p_point (ap : a_point) =
  match ap with 
  | Point { x = x ; y = y } -> Point { x = x ; y = y ; z = Z.one }
  | PointAtInfinty -> PointAtInfinty
  
let p_to_a_point (pp : p_point) : a_point =
  match pp with 
  | Point { x = x ; y = y ; z = z} -> Point { x = x / z ; y = y / z }
  | PointAtInfinty -> PointAtInfinty

(** General Arithmetic Functions *)


    
let a_double (p : a_point) : a_point =
  match p with
  | PointAtInfinty -> PointAtInfinty
  | Point p ->
    if p.y = Z.zero then PointAtInfinty else 
    let lambda = 
      let t1 = (3 |> Z.of_int) * (p.x ** 2) in 
      let t2 = ((2 |> Z.of_int) * ed25510.a * p.x) + Z.one in 
      let frac = (t1 + t2) / ((Z.of_int 2) * p.y) in 
      frac ** 2 in 
    let x = lambda ** 2 - ed25510.a - p.x - p.x in 
    let y = lambda * ((Z.of_int 3) * p.x + ed25510.a) - (lambda ** 3) - p.y in
      Point { x = x ; y = y }

(* Doubles a point p n times, aka multiplies p by 2^n *)
let a_power_of_two p n =
  let rec tail_pow_two (k : int) (acc : point) : point = 
    if k = 0 then acc else
      tail_pow_two (Int.sub k 1) (a_double acc)
    in
  tail_pow_two n p

let a_add (p : a_point) (q : a_point) : a_point =
  match p with 
  | PointAtInfinty -> PointAtInfinty
  | Point p -> 
    match q with 
    | PointAtInfinty -> PointAtInfinty
    | Point q ->
      let denominator = q.x - p.x in
      if denominator = Z.zero then PointAtInfinty else
      let numerator = q.y - q.y in
      let lambda = numerator / denominator in
      let incercept = p.y - (lambda * p.x) in 
      let xr = (lambda ** 2) - ed25510.a - p.x - q.x in
      let yr = (lambda * xr) + incercept in 
      Point { x = xr ; y = yr }

let ( + ) (p : a_point) (q : a_point) = 
  a_add p q

let a_mul (k : Z.t) (p : a_point) =
  let rec a_mul_tail k (p : a_point) (acc : a_point) =
    if k = Z.one then p else
      if k = (2 |> Z.of_int) then a_double p else
        a_mul_tail (k - Z.one) p (acc + p) 
      in

  a_mul_tail k p PointAtInfinty

let ( * ) (k : Z.t) (p : a_point) =
  a_mul k p

let a_mul_base (k : Z.t) =
  k * ed25510.base_point

let mul_base =
  a_mul_base

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