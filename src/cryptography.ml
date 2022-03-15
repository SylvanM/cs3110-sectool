open Elliptic_curve
open Z

let generate_secret f n p =
    (multiply_point f n p) mod f.p