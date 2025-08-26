type token = 
    | REAL of int
    | EOF 
    | ADD
    | SUB
    | MUL
    | DIV 

(* type ast =  *)
(*   | Leaf *)
(*   | Node of token * ast * ast *)

type ast = string list

module Tokens = struct
    let to_string = function
        | REAL d -> Printf.sprintf "REAL(%d)" d
        | EOF -> "EOF"
        | ADD -> "ADD"
        | SUB -> "SUB"
        | MUL -> "MUL"
        | DIV -> "DIV"

    let is_op t = 
      List.exists t [ADD; SUB; MUL; DIV] 

    let parse_tokens (tokens: token list): ast =
        tokens |> List.map (fun token -> 
          match token with
          | REAL d -> Printf.sprintf "Processing REAL token with value: %d" d
          | EOF -> "Processing EOF token"
          | ADD -> "Processing ADD token"
          | SUB -> "Processing SUB token"
          | MUL -> "Processing MUL token"
          | DIV -> "Processing DIV token"
        )
    

    let atoi c = 
      int_of_char c - int_of_char '0'

    let tokenize (s: string) : token list =
      String.to_seq s 
      |> List.of_seq 
      |> List.map (fun c -> 
        match c with
        | '+' -> ADD
        | '-' -> SUB
        | '*' -> MUL
        | '/' -> DIV
        | ('0'..'9') as c -> REAL (atoi c)
        | c -> failwith ("Unknown Token " ^ String.make 1 c) 
      )
      

end
