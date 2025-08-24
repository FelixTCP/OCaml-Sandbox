open Tokens

let end_program () =
    print_endline "Thanks for using the calculator!";
    exit 0
;;

let read_line () =
  try Some(read_line())
  with End_of_file -> None

let rec read_new_line () =
  match read_line () with
  | Some line -> process_input line
  | None -> end_program ()

and process_input input =
  if input = "exit" then
    end_program ()
  else begin
    print_endline ("You entered: " ^ input);
    Tokens.tokenize input |> List.iter (fun token ->
      print_endline (Tokens.to_string token);
      print_endline (Tokens.process_token token)
    );
    read_new_line ()
  end


let start_program () =
    print_endline "Welcome to the simple calculator!";
    print_endline "Type 'exit' or press 'Ctrl+D' to quit.";
    read_new_line ()
;;

let () = start_program ()


