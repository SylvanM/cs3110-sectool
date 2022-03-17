open Elliptic_curve

let generate_secret f n p =
    let point = multiply_point f n p in
        {x=point.x mod f.p ; y=point.x mod f.p}