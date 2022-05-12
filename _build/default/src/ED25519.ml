open Z

(* Represents an Affine point on an EC *)
type a_point = Point of { x : Z.t ; y : Z.t } | PointAtInfinty

(* Represents a proejctive point on an EC *)
type p_point = Point of { x : Z.t ; z : Z.t} | PointAtInfinty

exception InfinityBasePoint

let a_to_p_point (ap : a_point) =
  match ap with
  | Point { x = x ; y = y } -> Point { x = x ; z = Z.one }
  | PointAtInfinty -> PointAtInfinty

type point = p_point



(* The type representing a montgomery curve *)
type m_curve = {
  modulus : Z.t ; (* Size of the field (prime in this case) *)
  a : Z.t ; (* coefficient a of the curve *)
  order : Z.t ;
  base_point_x : Z.t ;
}

let ed25519 : m_curve = {
  modulus = (Z.pow (2 |> Z.of_int) (255)) - (19 |> Z.of_int) ; (* The size of the field *)
  a = 486662 |> Z.of_int ; (* The constant a *)
  order = (Z.pow (2 |> Z.of_int) (252) ) + ( "27742317777372353535851937790883648493" |> Z.of_string) ;
  base_point_x = "15112221349535400772501151409588531511454012693041857206046113283949847762202" |> Z.of_string_base 10 ;
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

(** General Arithmetic Functions *)

(** Returns the affine x coordinate of a projective point p *)
let get_x_coord (p : point) =
  match p with
  | PointAtInfinty -> Z.minus_one
  | Point p -> p.x / p.z

let p_double p =
  match p with 
  | PointAtInfinty -> PointAtInfinty
  | Point p ->
    if p.z = zero then PointAtInfinty else 
      let x_2i = ((p.x **2) - (p.z ** 2)) ** 2 in 
      let z_2i = 
        let t1 = (Z.of_int 4) * p.x * p.z in 
        let t2 = p.x ** 2 in 
        let t3 = ed25519.a * p.x * p.z in 
        let t4 = p.z ** 2 in 
        t1 * (t2 + t3 + t4) in 
      Point {
        x = x_2i ;
        z = z_2i ;
      }

let p_add pi pi1 =
  match pi with 
  | PointAtInfinty -> pi1 
  | Point pi -> 
    match pi1 with 
    | PointAtInfinty -> Point pi 
    | Point pi1 ->
      let x_2i1 = 
        let t1 = pi.x * pi1.x in 
        let t2 = pi.z * pi1.z in 
        (t1 - t2) ** 2 in 
      let z_2i1 =
        let t1 = pi.x * pi1.z in 
        let t2 = pi1.x * pi.z in 
        ed25519.base_point_x * ((t1 - t2) ** 2) in 
      Point {
        x = x_2i1 ;
        z = z_2i1 ;
      }

let rec ladder p k = 
  if k = zero then 
    (PointAtInfinty, p)
  else 
    let i = Z.shift_right_trunc k 1 in 
    let (pi, pi1) = ladder p i in 
  if is_even k then
    (p_double pi, p_add pi pi1)
  else 
    (p_add pi pi1, p_double pi1)

let p_mul (k : Z.t) (p : point) =
  let (pk, _) = ladder p k in pk

let ( + ) (p : point) (q : point) =
  p_add p q

let make_point (p : Z.t * Z.t) : p_point =
  match p with
  | (x, z) -> Point {x = x ; z = z}

let string_of_point (p : point) =
  match p with
  | PointAtInfinty -> "Inf"
  | Point p -> (p.x |> Z.to_string)