(*
Name: Aaryan Sharma
Program - 5 : SimpleC Type Checking
CS - 341 (Spring 2024)
Professor: Ellen Kidane
*)

namespace compiler

module checker =

  let private errorMessage a b (c:int) =                                                            // This helper function generates error messages based on error codes(taken as parameters).  
    match c with
    | 0 -> failwith "Unexpected end of input"
    | 1 -> failwith ("expecting " + a + ", but found " + b)
    | 2 -> failwith ("redefinition of variable '" + a + "'")
    | 3 -> failwith ("variable '" + a + "' undefined")
    | _ -> failwith "Unknown error"


  let private accessHd list =                                                                     // This helper function accesses the head of "list".
    match list with
    | hd :: tl -> hd
    | [] -> ""


  let private accessTl list x =                                                                     // This helper function accesses the tail of "list".
    match list with
    | hd :: tl -> tl
    | [] -> [""]


  let private memo tokens ele =                                                 // This helper function implements memoization, to determine matches of "ele".
    match tokens with
    | hd :: tl -> List.contains hd ele
    | [] -> false


  let rec stringLength s acc =
    match s with
    | "" -> acc
    | _ -> stringLength (s.Substring(1)) (acc + 1)


  let rec private contains symboltable id =                                                         // This helper function checks if a given identifier is present in the symbol table.
    match symboltable with 
    | [] -> false
    | (hd1 , hd2) :: tl when id = hd1 -> true
    | (hd1 , hd2) :: tl -> contains tl id


  let helperName (identifierString: string) =                                                       // This helper function extracts the name of the identifier from the string.
    let parts = identifierString.Split(':')
    parts.[1]


  let private matchToken expected_token tokens =                                                   // This function was given.                                             

    match tokens with
    | [] -> errorMessage "" "" 0
    | next :: rest when expected_token = next -> rest
    | next :: _ -> errorMessage expected_token next 1 


  let helperStartWith (pattern: string) (literal: string) =                                         // This helper function checks if a string starts with a given pattern.                                            
    literal.StartsWith(pattern)


  let private matchIdentifier tokens =                                                              // This helper function matches an identifier. 

    match tokens with
    | [] -> errorMessage "" "" 0
    | next :: rest when (helperStartWith "identifier:" next ) -> rest 
    | next :: rest when (helperStartWith "int_literal:" next ) -> rest
    | next :: rest when (helperStartWith "real_literal:" next ) -> rest
    | next :: rest when (helperStartWith "str_literal:" next ) -> rest
    | next :: _ -> errorMessage "identifier" next 1


  let private vardecl T1 symboltable = 
    match T1 with
    | "int" :: _ -> 
        T1
        |> matchToken "int"
        |> matchIdentifier
        |> matchToken ";"
        
    | "real" :: _ -> 
        T1
        |> matchToken "real"
        |> matchIdentifier
        |> matchToken ";"

    | _ -> T1


  let private operator token (c: int) = 

    match c with
      | 0 -> 
          token = "+" || token = "-" || token = "*" || token = "/" || token = "^" || token = "<" || token = "<=" || token = ">" || token = ">=" || token = "==" || token = "!="

      | 1 -> 
          token = "+" || token = "-" || token = "*" || token = "/" || token = "^"

      | 2 ->
          token = "<"
          || token = "<=" || token = ">" || token = ">=" || token = "==" || token = "!="
      | _ -> false


  let private exprValue T1 symboltable=                                                             // This helper function parses and handles values of expressions.
    let hd1 = List.head T1
    
    match T1 with
    | hd :: _ when (helperStartWith "identifier:" hd) -> 
        let parts = hd1.Split(':')
        let varName = parts.[1]
        let (_, varType) = List.find (fun (x, _) -> x = varName) symboltable
        (matchIdentifier T1, varType)

    | hd :: _ when (helperStartWith "int_literal:" hd) -> (matchIdentifier  T1, "int")
    | hd :: _ when (helperStartWith "real_literal:" hd) -> (matchIdentifier T1, "real")
    | hd :: _ when (helperStartWith "str_literal:" hd) -> (matchIdentifier T1, "str")
    | "true" :: _ -> (matchToken "true" T1, "bool")
    | "false" :: _ -> (matchToken "false" T1, "bool")
    | next :: _ -> failwith ("expecting identifier or literal, but found " + next)
    | _ -> (T1, "")


  let private exprOp T1 symboltable =                                                               // This helper function parses and handles expressions that are operators.
    let operators = ["+"; "-"; "*"; "/"; "^"; "<"; "<="; ">"; ">="; "=="; "!="]
    let token_hd = List.head T1

    if List.contains token_hd operators then
        matchToken token_hd T1
    else
        errorMessage token_hd "" 1


  let private expr T1 symboltable =                                                                 // This function parses and handles complete expressions.

    let (T2, Type_variable1 )  = exprValue T1 symboltable
    let T5 = List.head T2

    if (operator T5 0) then
      let T3 = exprOp T2 symboltable

      let (T4, Type_variable2) = exprValue  T3 symboltable

      if (T5 = "==") && (Type_variable1 = "real" || Type_variable2 = "real") then
        printfn "warning: comparing real numbers with == may never be true"


      if (operator T5 1) && not ((Type_variable1 = "int" || Type_variable2 = "real") && (Type_variable1.Equals Type_variable2)) then
        failwith ("operator " + T5 + " must involve 'int' or 'real'")


      if (operator T5 2) && not (Type_variable1.Equals Type_variable2) && not ((Type_variable1 = "true" || Type_variable1 = "false")  && (Type_variable2 = "false" || Type_variable2 = "true")) then
        failwith("type mismatch '" + Type_variable1 + "' " + T5 + " '" + Type_variable2 + "'")
      
      if (operator T5 2) then
        (T4, "bool")
      
      else
        (T4, Type_variable1)
    
    else
      (T2, Type_variable1)



  let private outputVal T1 symboltable =                                                            // This helper function parses and handles output values.
    let nextToken = List.head T1
    if nextToken = "endl" then
      matchToken "endl" T1
    else
      let (T2, varType) = exprValue T1 symboltable
      T2


  let private output T1 symboltable =                                                        // This function parses and handles output statements. 
    let T2 = matchToken "cout" T1
    let T3 = matchToken "<<" T2
    let T4 = outputVal T3 symboltable
    matchToken ";" T4
  

  let private assignment T1 symboltable = 

    let hd1 = List.head T1
    let nameofvar = helperName hd1
    let typeofvar = 
        let (_, varType) = List.find (fun (x, _) -> x = nameofvar) symboltable
        varType

    let T2 = matchIdentifier T1
    let T3 = matchToken "=" T2
    let (T4, typeofexpr)  = expr T3 symboltable

    if not (typeofvar.Equals typeofexpr) && not (typeofexpr = "int" && typeofvar = "real") then
      failwith("cannot assign '" + typeofexpr + "' to variable of type '" + typeofvar + "'")
    
    matchToken ";" T4
    

  let rec private stmt T1 symboltable = 
      
      match T1 with 
      | ";" :: tl -> matchToken ";" T1
      | "int" :: tl ->  vardecl T1 symboltable
      | "real" :: tl ->  vardecl T1 symboltable
      | "cin" :: tl -> 
          let T2 = matchToken "cin" T1
          let T3 = matchToken ">>" T2
          let T4 = matchIdentifier T3
          matchToken ";" T4
          
      | "cout" :: tl -> output T1 symboltable
      | "if" :: tl -> ifstmt T1 symboltable
      | hd :: _ when helperStartWith "identifier:" hd -> assignment T1 symboltable
      | next :: _ -> failwith ("expecting statement, but found " + next) 
      |  _ -> T1


  and private helperIf T1 symboltable =                                                             // This helper function parses and handles the "if" statement. 
    stmt T1 symboltable


  and private helperElse T1 symboltable =                                                           // This helper function parses and handles the "else" statement.
    let next = List.head T1

    if next = "else" then
      let T2 = matchToken "else" T1
      stmt T2 symboltable
    else
      T1


  and ifstmt T1 symboltable =                                                                       // This function parses and handles "if" statements.

    let T5 = 
        T1
        |> matchToken "if"
        |> matchToken "("
        |> (fun T4 -> expr T4 symboltable)
        |> fun (T4, con_type) ->
            if not (con_type = "bool") then
                failwith("if condition must be 'bool', but found '" + con_type + "'")
            T4
        |> matchToken ")"
    
    let T6 = helperIf T5 symboltable
    helperElse T6 symboltable


  let rec private morestmts T1 symboltable =
    if accessHd T1 = "}" then
        T1
    
    else
        let T2 = stmt T1 symboltable
        morestmts T2 symboltable



  let rec private stmts T1 symboltable =
    let T2 = stmt T1 symboltable
    let T3 = morestmts T2 symboltable
    T3



  let private simpleC tokens symboltable =
    
    tokens
    |> matchToken "void"
    |> matchToken "main"
    |> matchToken "("
    |> matchToken ")"
    |> matchToken "{"
    |> (fun t -> stmts t symboltable)
    |> matchToken "}"
    |> matchToken "$"


  //
  // typecheck T1 symboltable
  //
  // Given a list of T1 and a symbol table, type-checks 
  // the program to ensure program's variables and expressions
  // are type-compatible. If the program is valid, returns 
  // the string "success". If the program contains a semantic
  // error or warning, returns a string of the form
  // "type_error: ...".
  //
  let typecheck T1 symboltable = 
    try
      let T2 = simpleC T1 symboltable
      "success"
    with 
      | ex -> "type_error: " + ex.Message