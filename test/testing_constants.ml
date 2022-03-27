(*************
* A list of constants to be used in tests, so we don't clutter up the
other file
*)
open Sectool.Elliptic_curve
open Z

let curve25519 = create_field [
  ( (Z.pow (2 |> Z.of_int) (255)) - (19 |> Z.of_int) ) ; (* prime field number *)
  ( 1 |> Z.of_int ) ;
  ( 486662 |> Z.of_int ) ;
  ( 1 |> Z.of_int ) ;
  ( 0 |> Z.of_int ) ;
  ( "15112221349535400772501151409588531511454012693041857206046113283949847762202" |> Z.of_string ) ; (* Base point x = 9 *)
  ( "46316835694926478169428394003475163141307993866256225615783033603165251855960" |> Z.of_string ) ; (* TODO actually find this point *)
  ( (Z.pow (2 |> Z.of_int) (252) ) + ( "27742317777372353535851937790883648493" |> Z.of_string) ) ; (* Order of base point x = 9 *)
  ( 8 |> Z.of_int ) (* Co-factor *)
]

let curve25519_G = make_point (
  "15112221349535400772501151409588531511454012693041857206046113283949847762202" |> Z.of_string_base 10,
  "46316835694926478169428394003475163141307993866256225615783033603165251855960" |> Z.of_string_base 10
)

let curve25519_2G = make_point (
  "24727413235106541002554574571675588834622768167397638456726423682521233608206" |> Z.of_string_base 10,
  "15549675580280190176352668710449542251549572066445060580507079593062643049417" |> Z.of_string_base 10
)

let simple_curve = create_field [
  ( 7 |> Z.of_int ) ;
  Z.one ;
  Z.one ;
  Z.minus_one ;
  Z.one ;
  Z.one ;
  Z.one ;
  Z.one ;
  Z.one ;
]

let example_curve = create_field [
  13 |> Z.of_int ;
  Z.one ;
  Z.zero ;
  5 |> Z.of_int ;
  4 |> Z.of_int ;
  Z.one ; (* dummy *)
  6 |> Z.of_int ;
  17 |> Z.of_int ; (* dummy *)
  5 |> Z.of_int ; (* dummy *)
]

let heewon_field = create_field [
  100 |> Z.of_int ;
  2 |> Z.of_int ;
  5 |> Z.of_int ;
  2 |> Z.of_int ;
  4 |> Z.of_int ;
  9 |> Z.of_int ;
  0 |> Z.of_int ;
  252 |> Z.of_int ;
  8 |> Z.of_int
]

let p192 = create_field [
  "6277101735386680763835789423207666416083908700390324961279" |> Z.of_string ;
  Z.zero ;
  Z.one ;
  -3 |> Z.of_int ;
  "64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1" |> Z.of_string_base 16 ;

  (* Dummy numbers *)
  Z.one ;
  6 |> Z.of_int ;
  Z.zero ;
  Z.zero ;
]

let p192_P = make_point (
  "188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012" |> Z.of_string_base 16,
  "07192b95ffc8da78631011ed6b24cdd573f977a11e794811" |> Z.of_string_base 16
)

let p192_Q = make_point (
  "5701b8be342fb767752f13a308e2eff016b41fd348ef1ea" |> Z.of_string_base 16,
  "77aeacae8fd493a524b9b18509c9a60e7e2a7da86882d82c" |> Z.of_string_base 16
)

let p192_R = make_point (
  "c5675f8265cf98e933db304666558478ca70c5ebba4da630" |> Z.of_string_base 16,
  "2c2560e527695bbe883084abf6736e0a7e06b489ba57cb39" |> Z.of_string_base 16
)

let p192_2Q = make_point (
  "dafebf5828783f2ad35534631588a3f629a70fb16982a888" |> Z.of_string_base 16,
  "dd6bda0d993da0fa46b27bbc141b868f59331afa5c7e93ab" |> Z.of_string_base 16
)