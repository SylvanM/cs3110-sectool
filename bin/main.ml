type path = String of string | Int of int | Cancel

type args = string list

let take_input s =
    match int_of_string s with
    | i -> Int i
    | exception e ->
        (match s with
        | "cancel" | "quit" -> Cancel
        | _ -> String s)

let rec take_string (msg:string) (acc:args) : args =
    print_string ("\n " ^ msg ^ "\n \n > ");
    match take_input (read_line ()) with
    | String s ->
        acc @ [s]
    | Int i ->
        print_endline "\n That's not a character string!";
        take_string msg acc
    | Cancel -> exit 0

let rec take_int (msg:string) (acc:args) : args =
    print_string ("\n " ^ msg ^ "\n \n > ");
    match take_input (read_line ()) with
    | Int i ->
        acc @ [string_of_int i]
    | String s ->
        print_endline "\n That's not an integer!";
        take_int msg acc
    | Cancel -> exit 0

let rec take_file (acc:args) (f:string -> string) (msg:string): args =
    print_string ("\n " ^ msg ^ "\n \n > ");
    match take_input (read_line ()) with
    | String s -> (
        match f s with
        | t -> acc @ [s]
        | exception e ->
            print_endline "\n Invalid file.";
            take_file acc f msg)
    | Int i ->
        print_endline "\n That's not a valid string!";
        take_file acc f msg
    | Cancel -> exit 0

let rec take_private_key (acc:args) : args =
    let f s =
    Sectool.File_wizard.read_private_key s
    |> Z.to_string
    in
    take_file acc f "Please enter a private key file:"

let rec take_public_key (acc:args) : args =
    let f s =
    Sectool.File_wizard.read_public_key s
    |> Sectool.ED25519.string_of_point
    in
    take_file acc f "Please enter a public key file:"

let rec command input =
    print_string "\n > ";
    match read_line () with
    | exception Exit -> exit 0
    | phrase -> (
      match Sectool.Command.parse phrase with
        | Read t ->
            let file = String.concat "" t in
            (match Sectool.File_wizard.read_file file with
                | s -> (
                    match List.length s with
                    | 1 -> print_endline (
                        "\n" ^
                        (Sectool.File_wizard.read_private_key file
                        |> Z.to_string))
                    | 2 -> print_endline (
                        "\n" ^
                        (Sectool.File_wizard.read_public_key file
                        |> Sectool.ED25519.string_of_point))
                    | _ ->
                        print_endline "\n Incorrectly formatted file! \n";
                    )
                | exception Sectool.Command.Malformed ->
                    print_endline "\n Incorrectly formatted file!";
                | exception Sectool.File_wizard.FileDoesNotExist s ->
                    print_endline "\n That file doesn't exist!";
            );
            command input
        | Private ->
            let list =
                []
                |> take_int "Please enter a bitsize:"
                |> take_string "Please enter the name of the file to write to:"
            in
            let size = List.hd list |> int_of_string in
            let name = List.nth list 1 in
            let result = Sectool.Ecdh.generate_private_key size in
            let result_string = result |> Z.to_string in
            Sectool.File_wizard.write_private_key result name;
            print_endline (
                "\n Successfully stored private key ["
                ^ result_string ^ "] in file [" ^ name ^ "]! \n");
            command input
        | Public ->
            let list =
                []
                |> take_private_key
                |> take_string "Please enter the name of the file to write to:"
            in
            let private_key = List.hd list |> Sectool.File_wizard.read_private_key in
            let name = List.nth list 1 in
            let result = Sectool.Ecdh.compute_public_key private_key in
            let result_string = Sectool.ED25519.string_of_point result in
            Sectool.File_wizard.write_public_key result name;
            print_endline (
                "\n Successfully stored public key ["
                ^ result_string ^  "] in file [" ^ name ^ "]! \n");
            command input
        | Secret ->
            let list =
                []
                |> take_private_key
                |> take_public_key
                |> take_string "Please enter the name of the file to write to:"
            in
            let private_key = List.hd list |> Sectool.File_wizard.read_private_key in
            let public_key = List.nth list 1 |> Sectool.File_wizard.read_public_key in
            let name = List.nth list 2 in
            let result = Sectool.Ecdh.compute_shared_secret private_key public_key in
            let result_string = Sectool.ED25519.string_of_point result in
            Sectool.File_wizard.write_public_key result name;
            print_endline (
                "\n Successfully stored secret ["
                ^ result_string ^ "] in file [" ^ name ^ "]! \n");
            command input
        | Quit -> exit 0
        | exception Sectool.Command.Malformed ->
            print_endline "\n Oops! That's not a valid command. Try again. \n";
            command input;
        | _ -> exit 0
    )

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\n This is the ECC Security Tool. \n";
  print_endline
    " Please enter a command:";
  command input

(* Execute the game engine. *)
let () = main ()
