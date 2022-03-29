(*************
* A list of constants to be used in tests, so we don't clutter up the
other file
*)
open Sectool.ED25519
open Z


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