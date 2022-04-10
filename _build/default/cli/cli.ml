open Sectool
open Cmdliner
open File_wizard

(* Implementation of the command, we just print the args. *)

(**
type prompt = Public | Private | Secret

let prompt_str = function
| Public -> "public" | Private -> "private" | Secret -> "secret"


let rm prompt lines files =
  let n = files |> List.length in
  (match prompt with
  | Private ->
    if n = 2 then
      let bitsize = List.hd files in
      let name = List.nth files 1 in
      (match int_of_string bitsize with
      | bitsize ->
        let result = Sectool.Ecdh.generate_private_key bitsize in
        let result_string = result |> Z.to_string in
        Sectool.File_wizard.write_private_key result name;
        Printf.printf "done!";
      | exception e -> failwith "Invalid bitsize."
      )
    else failwith "Invalid format."
  | Public -> Printf.printf "public"
  | Secret -> Printf.printf "secret");
  Printf.printf " prompt = %s\n files = %s\n lines = %s\n length = %s\n"
    (prompt_str prompt) (String.concat ", " files) (string_of_int lines) (files |> List.length |> string_of_int)

(* Command line interface *)

let files = Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE")

let lines =
  let doc = "Output the last $(docv) lines or use $(i,+)$(docv) to start \
             output after the $(i,N)-1th line."
  in
  Arg.(value & & info ["n"; "lines"] ~docv:"N" ~doc)


let prompt =
  let always =
    let doc = "generate public key" in
    Public, Arg.info ["i"; "pub"; "public"] ~doc
  in
  let never =
    let doc = "generate private key" in
    Private, Arg.info ["f"; "priv"; "private"] ~doc
  in
  Arg.(last & vflag_all [Public] [always; never])

let cmd =
  let doc = "Remove files or directories" in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) removes each specified $(i,FILE). By default it does not
        remove directories, to also remove them and their contents, use the
        option $(b,--recursive) ($(b,-r) or $(b,-R)).";
    `P "To remove a file whose name starts with a $(b,-), for example
        $(b,-foo), use one of these commands:";
    `Pre "$(mname) $(b,-- -foo)"; `Noblank;
    `Pre "$(mname) $(b,./-foo)";
    `P "$(tname) removes symbolic links, not the files referenced by the
        links.";
    `S Manpage.s_bugs; `P "Report bugs to <bugs@example.org>.";
    `S Manpage.s_see_also; `P "$(b,rmdir)(1), $(b,unlink)(2)" ]
  in
  let info = Cmd.info "rm" ~version:"%%VERSION%%" ~doc ~man in
  Cmd.v info Term.(const rm $ prompt $ lines $ files)

let main () = exit (Cmd.eval cmd)
let () = main ()
*)

(**
let revolt () = print_endline "Revolt!"

open Cmdliner

let revolt_t = Term.(const revolt $ const ())

let cmd = Cmd.v (Cmd.info "revolt") revolt_t

let () = exit (Cmd.eval cmd)
*)



(*
   Source code for the cmdliner-cheatsheet executable.
   This can serve as a template for new programs.
   This sample program exercises various features of cmdliner. It
   can be compiled and run to check how the various options work.
   See also 'Demo_subcmd_main.ml' for how to implement subcommands.
*)

open Printf

(* Provide the 'Arg', 'Term', and 'Manpage' modules. *)
open Cmdliner

(*
   We store the result of parsing the command line into a single 'conf'
   record of the following type.
*)


type private_conf = {
  bitsize: int;
  name: string option;
}

type public_conf = {
  private_input: string option;
  name: string option;
}

type secret_conf = {
  private_input: string option;
  public_input: string option;
  name: string option;
}

type cmd_conf =
  | Private of private_conf
  | Public of public_conf
  | Secret of secret_conf

(*
   The core of the application.
*)

let opt_string opt =
  match opt with
  | None -> "none"
  | Some s -> sprintf "%S" s

let run cmd_conf =
  match cmd_conf with
  | Private conf ->
      printf
      "\
        Private configuration:
        bitsize: %i
        name: %s
"
      conf.bitsize
      (opt_string conf.name)
  | Public conf ->
      printf
      "\
        Public configuration:
        private input: %s
        name: %s
"
      (opt_string conf.private_input)
      (opt_string conf.name)
  | Secret conf ->
      printf
      "\
        Public configuration:
        private input: %s
        public input: %s
        name: %s
"
      (opt_string conf.private_input)
      (opt_string conf.public_input)
      (opt_string conf.name)

(************************* Command-line management *************************)

(*
   For each kind of command-line argument, we define a "term" object.
*)

let filename_term =
  let info =
    Arg.info []
      ~docv:"NAME"
      ~doc:"Example of an optional argument without a default."
  in
  Arg.value (Arg.pos 0 (Arg.some Arg.string) None info)

let private_input_term =
  let info =
    Arg.info ["p";"priv";"private"]  (* list must be empty for anonymous arguments *)
      ~docv:"FILE"
      ~doc:"Example of an anonymous argument at a fixed position."
  in
  Arg.value (Arg.opt (Arg.some Arg.file) None info)

let public_input_term =
  let info =
    Arg.info ["P";"pub";"public"]  (* list must be empty for anonymous arguments *)
      ~docv:"FILE"
      ~doc:"Example of an anonymous argument at a fixed position."
  in
  Arg.value (Arg.opt (Arg.some Arg.file) None info)

let bitsize_term =
  let default = 1 in
  let info =
    Arg.info ["n"; "bitsize"]  (* '-j' and '--num-cores' will be synonyms *)
      ~docv:"NUM"
      ~doc:"Example of an optional argument with a default.
            The value of \\$\\(docv\\) is $(docv)."
  in
  Arg.value (Arg.opt Arg.int default info)

(*
   Combine the values collected for each kind of argument into a single
   'conf' object, which is then passed to our main 'run' function.
   Some merging and tweaking can be useful here but most often
   we just map each argument to its own record field.
*)
let private_term run =
  let combine name bitsize =
    let conf = Private {
      bitsize;
      name
    } in
    run conf
  in
  Term.(const combine
        $ filename_term
        $ bitsize_term
       )

let private_doc = "[some headline for private]"

let private_man = [
  `S Manpage.s_description;
  `P "[multiline overview of private]";
]

let private_cmd run =
  let info =
    Cmd.info "private"
      ~doc:private_doc
      ~man:private_man
  in
  Cmd.v info (private_term run)

(*** Putting together subcommand 'subcmd2' ***)

let public_term run =
  let combine name private_input =
    let conf = Public {
      private_input;
      name
    } in
    run conf
  in
  Term.(const combine
        $ filename_term
        $ private_input_term
       )

let public_doc = "[some headline for public]"

let public_man = [
  `S Manpage.s_description;
  `P "[multiline overview of public]";
]

let public_cmd run =
  let info =
    Cmd.info "public"
      ~doc:public_doc
      ~man:public_man
  in
  Cmd.v info (public_term run)

(*** Putting together subcommand 'subcmd2' ***)

let secret_term run =
  let combine name private_input public_input =
    let conf = Secret {
      private_input;
      public_input;
      name
    } in
    run conf
  in
  Term.(const combine
        $ filename_term
        $ private_input_term
        $ public_input_term
       )

let secret_doc = "[some headline for secret]"

let secret_man = [
  `S Manpage.s_description;
  `P "[multiline overview of secret]";
]

let secret_cmd run =
  let info =
    Cmd.info "secret"
      ~doc:secret_doc
      ~man:secret_man
  in
  Cmd.v info (secret_term run)

(*** Putting together the main command ***)

let root_doc = "[some headline for the main command]"

let root_man = [
  `S Manpage.s_description;
  `P "[multiline overview of the main command]";
]

(*
   Use the built-in action consisting in displaying the help page.
*)
let root_term =
  Term.ret (Term.const (`Help (`Pager, None)))

let root_info =
  Cmd.info "sectool-cli"
    ~doc:root_doc
    ~man:root_man

(*** Parse the command line and do something with it ***)

let subcommands run = [
  private_cmd run;
  public_cmd run;
  secret_cmd run;
]

(*
   Parse the command line into a 'conf' record and pass it to the
   main function 'run'.
*)

let parse_command_line_and_run (run : cmd_conf -> unit) =
  Cmd.group root_info (subcommands run) |> Cmd.eval |> exit

let safe_run conf =
  try run conf
  with
  | Failure msg ->
      eprintf "Error: %s\n%!" msg;
      exit 1
  | e ->
      let trace = Printexc.get_backtrace () in
      eprintf "Error: exception %s\n%s%!"
        (Printexc.to_string e)
        trace

let main () =
  Printexc.record_backtrace true;
  parse_command_line_and_run safe_run

let () = main ()
