open Stream

type t = 
    | REAL of float
    | VAR of string
    | EOF 
    | ADD
    | SUB
    | MUL
    | DIV 
    | PAR_LEFT
    | PAR_RIGHT
    | POW
    | SIN
    | COS
    | TAN
    | LOG
    | EXP
    | SQRT
    | FACT

module Tokens = struct
    let to_string = function
        | REAL f -> Printf.sprintf "REAL(%f)" f
        | VAR v -> Printf.sprintf "VAR(%s)" v
        | EOF -> "EOF"
        | ADD -> "ADD"
        | SUB -> "SUB"
        | MUL -> "MUL"
        | DIV -> "DIV"
        | PAR_LEFT -> "PAR_LEFT"
        | PAR_RIGHT -> "PAR_RIGHT"
        | POW -> "POW"
        | SIN -> "SIN"
        | COS -> "COS"
        | TAN -> "TAN"
        | LOG -> "LOG"
        | EXP -> "EXP"
        | SQRT -> "SQRT"
        | FACT -> "FACT"

    let process_token token =
        match token with
        | REAL f -> Printf.sprintf "Processing REAL token with value: %f" f
        | VAR v -> Printf.sprintf "Processing VAR token with name: %s" v
        | EOF -> "Processing EOF token"
        | ADD -> "Processing ADD token"
        | SUB -> "Processing SUB token"
        | MUL -> "Processing MUL token"
        | DIV -> "Processing DIV token"
        | PAR_LEFT -> "Processing PAR_LEFT token"
        | PAR_RIGHT -> "Processing PAR_RIGHT token"
        | POW -> "Processing POW token"
        | SIN -> "Processing SIN token"
        | COS -> "Processing COS token"
        | TAN -> "Processing TAN token"
        | LOG -> "Processing LOG token"
        | EXP -> "Processing EXP token"
        | SQRT -> "Processing SQRT token"
        | FACT -> "Processing FACT token"

    let tokenize input =
        List.map (fun token ->
            match token with
            | "+" -> ADD
            | "-" -> SUB
            | "*" -> MUL
            | "/" -> DIV
            | "(" -> PAR_LEFT
            | ")" -> PAR_RIGHT
            | "^" -> POW
            | "sin" -> SIN
            | "cos" -> COS
            | "tan" -> TAN
            | "log" -> LOG
            | "exp" -> EXP
            | "sqrt" -> SQRT
            | "!" -> FACT
            | _ -> failwith ("Unknown token: " ^ token)
        ) 
end
