open Elliptic_curve

let generate_secret (f : Elliptic_curve.field) (n : int) (p : Elliptic_curve.point) : Elliptic_curve.point =
    multiply_point f n p