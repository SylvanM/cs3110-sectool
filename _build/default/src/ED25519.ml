open Z

(* The type representing a montgomery curve *)
type m_curve = {
  modulus : Z.t ; (* Size of the field (prime in this case) *)
  a : Z.t ; (* coefficient a of the curve *)
  order : Z.t ;
  base_point_x : Z.t ;
  base_point_z : Z.t ; 
  base_point_y : Z.t ;
}

let ed25519 : m_curve = {
  modulus = Z.sub (Z.pow (2 |> Z.of_int) (255)) (19 |> Z.of_int) ; (* The size of the field *)
  a = 486662 |> Z.of_int ; (* The constant a *)
  order = (Z.pow (2 |> Z.of_int) (252) ) + ( "27742317777372353535851937790883648493" |> Z.of_string) ;
  base_point_x = "15112221349535400772501151409588531511454012693041857206046113283949847762202" |> Z.of_string_base 10 ;
  base_point_z = one ;
  base_point_y = "46316835694926478169428394003475163141307993866256225615783033603165251855960" |> Z.of_string ;
}

(* This is the definition of in infix mod operator that will always produce a positive integer *)
let rec ( % ) x y =
	let m = x mod y in
		if m < zero then
			((x + y) % y)
		else
			m

(* let rec ( % ) = Z.rem *)

(** TEMPORARYYY *)



let add m a b =
  ((a % m) + (b % m)) % m

let sub m a b =
  ((a % m) - (b % m)) % m

(** Define multiplication over a modulus m *)
let mul m a b =
  ((a % m) * (b % m)) % m

let pow m b p =
  Z.powm b (p |> Z.of_int) m

let mul_inv m a =
  Z.invert a m

let div m a b =
  mul m a (mul_inv m b)


let ( + ) =
  add ed25519.modulus

let ( - ) =
  sub ed25519.modulus 

let ( * )  =
  mul ed25519.modulus 

let ( / )  =
  div ed25519.modulus 

let ( ** )  =
  pow ed25519.modulus 

module type PointRepresentation = sig

  type t

  val double : t -> t
  (** Adds a point to itself on a curve *)

  val inf : t 
  (** The point at infinity *)

  val add : t -> t -> t 
  (** Adds two points on Ed25519 *)

  val base : t 
  (** A representation of the base point *)

  val make : (Z.t * Z.t)-> t
  (** Makes a point from affine coordinates *)

  val get_x_coord : t -> Z.t
  (** Returns the affine x coordinate of this point *)

  val equals : t -> t -> bool 
  (** Returns true if the two points ought to be considered equal *)

end

module type FiniteFieldOperations = sig

  type point
  
  val mul : Z.t -> point -> point

  val base : point

  val make : (Z.t * Z.t) -> point

  val equals : point -> point -> bool

  val get_x_coord : point -> Z.t

end

module ED25519Operations (P : PointRepresentation) : FiniteFieldOperations = struct

  type point = P.t

  let base = P.base

  let get_x_coord = P.get_x_coord 

  let equals = P.equals

  let double = P.double

  let add = P.add

  let rec ladder (k : Z.t) (p : point) = 
    if k = zero then 
      (P.inf, p)
    else 
      let i = Z.shift_right_trunc k 1 in 
      let (pi, pi1) = ladder i p in 
    if is_even k then
      (double pi, add pi pi1)
    else 
      (add pi pi1, double pi1)

  let ladder_mul k p =
    let (pk, _) = ladder k p in pk

  let mul = ladder_mul

  let make = P.make



end

module AffinePoint : PointRepresentation = struct

  type t = Point of { x : Z.t ; y : Z.t } | PointAtInfinty

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
  let compute_next x1 y1 x2 y2 =
    let l = lambda x1 y1 x2 y2 in
    let x = (l ** 2) - ed25519.a - x1 - x2 in
    let y = l * (x1 - x) - y1 in
    (x, y)

  let double p : t =
    match p with
    | PointAtInfinty -> PointAtInfinty
    | Point p ->
      if p.y = Z.zero then PointAtInfinty else
      let (x, y) = compute_next p.x p.y p.x p.y in
      Point {
        x = x ;
        y = y
      }

  (** Adds a point to itself on a curve *)

  let inf = PointAtInfinty

  let add p q =
    if p = q then double p else
    match p with
    | PointAtInfinty -> q
    | Point p ->
      match q with
      | PointAtInfinty -> Point p
      | Point q ->
        if p.x = q.x then PointAtInfinty else
          let (x, y) = compute_next p.x p.y q.x q.y in
          Point {
            x = x ;
            y = y ;
          }

  let make (x, y) = Point { x = x ; y = y }

  let base = make (ed25519.base_point_x, ed25519.base_point_y)
  (** A representation of the base point *)

  let get_x_coord = function 
    | PointAtInfinty -> minus_one
    | Point { x = x ; y = y } -> x

  let equals (p : t) (q : t) = 
    match (p, q) with 
    | (PointAtInfinty, PointAtInfinty) -> true 
    | (Point {x = x1 ; y = y1}, Point {x = x2 ; y = y2}) -> x1 = x2 
    | _ -> false 
  
end

module ProjectivePoint : PointRepresentation = struct

  type t = Point of { x : Z.t ; z : Z.t } | PointAtInfinty

  let inf = PointAtInfinty

  let get_x_coord p =
    match p with
    | PointAtInfinty -> Z.minus_one
    | Point p -> p.x / p.z


  let equals p1 p2 =
    get_x_coord p1 = get_x_coord p2

  let double p = 
    match p with 
    | PointAtInfinty -> PointAtInfinty
    | Point p ->
      if p.z = zero then PointAtInfinty else 
        let x_2i = ((p.x ** 2) - (p.z ** 2)) ** 2 in 
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

  let add pi pi1 =
    if (equals pi pi1) then double pi else
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

  (** Adds two points on Ed25519 *)

  let make (x, _) = Point { x = x ; z = one }

  let base = make (ed25519.base_point_x, one)

end

module M = ED25519Operations (AffinePoint)

type point = M.point

let get_x_coord = M.get_x_coord

let make_point = M.make

let string_of_point p = p |> M.get_x_coord |> Z.to_string

let ( * ) = M.mul

let base = M.base

let equals = M.equals