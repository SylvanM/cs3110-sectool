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
  ( 9 |> Z.of_int ) ; (* Base point x = 9 *)
  ( (Z.pow (2 |> Z.of_int) (252) ) + ( "27742317777372353535851937790883648493" |> Z.of_string) ) ; (* Order of base point x = 9 *)
  ( 8 |> Z.of_int ) (* Co-factor *)
]