let end_program () =
    print_endline "Thanks for using the calculator!";
    exit 0
;;

let evaluate_ast (ast) : int =
  (* Dummy evaluator: counts characters *)
        
  ast |> List.iter (fun str -> print_endline str);
  1

let print_result (result: int) : unit =
  Printf.printf "Result: %d\n" result

open Tokens
let rec main_loop () =
  print_string "> ";  (* prompt *)
  flush_all ();
  try
    let line = input_line stdin in
    if line = "exit" then
      end_program ()
    else
      (* Pipeline: *)
      line
      |> Tokens.tokenize           (* string -> token list *)
      |> Tokens.parse_tokens       (* token list -> AST *)
      |> evaluate_ast              (* AST -> result *)
      |> print_result;             (* result -> unit *)
      main_loop ()
  with
  | End_of_file -> end_program ()
  | e -> 
      Printf.printf "Error: %s\n" (Printexc.to_string e);
      main_loop ()

let start_program () =
    print_endline "Welcome to the simple calculator!";
    print_endline "Type 'exit' or press 'Ctrl+D' to quit.";

    main_loop ()
;;

let () = start_program ()


