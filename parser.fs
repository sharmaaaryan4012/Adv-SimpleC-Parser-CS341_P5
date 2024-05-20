(*
Name: Aaryan Sharma
Program - 5 : SimpleC Type Checking
CS - 341 (Spring 2024)
Professor: Ellen Kidane
*)

namespace compiler

module parser =

  let private errorMessage a b (c:int) =                                                            // This helper function generates error messages based on error codes(taken as parameters).
    
    match c with
    | 0 -> failwith "Unexpected end of input"
    | 1 -> failwith ("expecting " + a + ", but found " + b)
    | _ -> failwith "Unknown error"


  let private matchToken expected_token tokens =                                                   // This function was given.

    match tokens with
    | [] -> errorMessage "" "" 0
    | next :: rest when expected_token = next -> rest
    | next :: _ -> errorMessage expected_token next 1                   


  let helperStartWith (pattern: string) (literal: string) =                                         // This helper function checks if a string starts with a given pattern.    
    literal.StartsWith(pattern) 


  let private matchIdentifier tokens =                                                              // This helper function matches an identifier. 

    match tokens with
    | [] -> failwith "Unexpected end of input"
    | next :: rest when (helperStartWith "identifier:" next ) -> rest
    | next :: rest when (helperStartWith "int_literal:" next ) -> rest
    | next :: rest when (helperStartWith "real_literal:" next ) -> rest
    | next :: rest when (helperStartWith "str_literal:" next ) -> rest
    | next :: _ -> errorMessage "expecting identifier, but found " next 1


  let  private vardecl tokens =                                                                     // This helper function parses and handles variable declarations.

    match tokens with
    | "int" :: _ -> 
                    let tokens2 = matchToken "int" tokens
                    let tokens3 = matchIdentifier tokens2
                    matchToken ";" tokens3
                    
    | "real" :: _ -> 
                    let tokens2 = matchToken "real" tokens
                    let tokens3 = matchIdentifier tokens2
                    matchToken ";" tokens3
    | _ -> tokens


  let private helperInput tokens =                                                                  // This function parses and handles input statements.

    let tokens2 = matchToken "cin" tokens
    let tokens3 = matchToken ">>" tokens2
    let tokens4 = matchIdentifier tokens3
    matchToken ";" tokens4


  let private exprValue tokens =                                                                    // This helper function parses and handles values of expressions.
  
    match tokens with
    | hd :: _ when (helperStartWith "identifier:" hd) -> matchIdentifier tokens
    | hd :: _ when (helperStartWith "int_literal:" hd) -> matchIdentifier tokens
    | hd :: _ when (helperStartWith "real_literal:" hd) -> matchIdentifier tokens
    | hd :: _ when (helperStartWith "str_literal:" hd) -> matchIdentifier tokens
    | "true" :: _ -> matchToken "true" tokens                                                               
    | "false" :: _ -> matchToken "false" tokens
    | next :: _ -> errorMessage "expecting identifier or literal, but found " next 1
    | _ -> tokens


  let private exprOp tokens =                                                                       // This helper function parses and handles expressions that are operators.
    
    match tokens with
    | op :: _ when List.exists (fun x -> x = op) ["+"; "-"; "*"; "/"; "^"; "<"; "<="; ">"; ">="; "=="; "!="] ->
        
        let tokens = 
          matchToken op tokens
        exprValue tokens
    | _ -> tokens


  let private expr tokens =                                                                         // This function parses and handles complete expressions.
    let tokens = exprValue tokens
    exprOp tokens



  let private outputVal tokens =                                                                    // This helper function parses and handles values of output operations.
    
    match tokens with
    | "endl" :: _ -> matchToken "endl" tokens
    | _ -> expr tokens


  let private output tokens =                                                                       // This helper function parses and handles output statements which output values via "cout".
    
    tokens 
    |> matchToken "cout" 
    |> matchToken "<<" 
    |> outputVal 
    |> matchToken ";"


  let private assignment tokens =                                                                   // This helper function parses and handles assignment statements.
    let tokens2 = 
      matchIdentifier tokens

    let tokens3 = 
      matchToken "=" tokens2

    let tokens4 = 
      expr tokens3
    matchToken ";" tokens4


  let rec private stmt tokens =                                                                       // This recursive function parses and handles statements.

      match tokens with 
      | ";" :: tl -> matchToken ";" tokens
      | "int" :: tl ->  vardecl tokens
      | "real" :: tl ->  vardecl tokens
      | "cin" :: tl -> helperInput tokens
      | "cout" :: tl -> output tokens                                                
      | "if" :: tl -> ifstmt tokens
      | hd :: _ when (helperStartWith "identifier:" hd) -> assignment tokens
      | next :: _ -> errorMessage "expecting statement, but found " next 1
      |  _ -> tokens


  and private helperIf =                                                                            // This helper function parses and handles the "if" statement.
    stmt


  and private helperElse tokens =                                                                   // This helper function parses and handles the "else" statement.
    
    match tokens with
    | "else" :: tl ->
        let tokens2 = 
          matchToken "else" tokens
        stmt tokens2 
    | _ -> tokens 


  and ifstmt tokens =                                                                               // This helper function parses and handles the "if"/"else" statement by using the above defined helper functions.
    tokens
    |> matchToken "if"
    |> matchToken "("
    |> expr
    |> matchToken ")"
    |> helperIf
    |> helperElse


  let rec private morestmts tokens =                                                                // This helper function parses and handles multiple statements via the "stmt" function.

    match tokens with
    | "}" :: _ -> tokens
    | _ -> 
        let tokensAfterStmt = stmt tokens
        morestmts tokensAfterStmt


  let private stmts tokens =                                                                        // This helper function parses and handles multiple statements via the "stmt" and "morestmts" functions.

    let rest_list = stmt tokens
    morestmts rest_list


  let private simpleC tokens =                                                                      // This is the main function which parses and handles the SimpleC program.
    tokens
    |> matchToken "void"
    |> matchToken "main"
    |> matchToken "("
    |> matchToken ")"
    |> matchToken "{"
    |> stmts
    |> matchToken "}"
    |> matchToken "$"

  let parse tokens =                                                                             // This function was given.
    try
      let result = simpleC tokens
      "Success!"
    with 
    | ex -> "syntax_error: " + ex.Message