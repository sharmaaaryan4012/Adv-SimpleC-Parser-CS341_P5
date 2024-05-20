(*
Name: Aaryan Sharma
Program - 5 : SimpleC Type Checking
CS - 341 (Spring 2024)
Professor: Ellen Kidane
*)


namespace compiler

module analyzer =


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


  let rec private contains symboltable id =                                                         // This helper function checks if a given identifier is present in the symbol table.

    match symboltable with 
    | [] -> false
    | (hd1 , hd2) :: tl when id = hd1 -> true
    | (hd1 , hd2) :: tl -> contains tl id


  let private memo tokens ele =                                                 // This helper function implements memoization, to determine matches of "ele".
    match tokens with
    | hd :: tl -> List.contains hd ele
    | [] -> false


  let rec stringLength s acc =
    match s with
    | "" -> acc
    | _ -> stringLength (s.Substring(1)) (acc + 1)


  let private matchToken expected_token tokens =                                                   // This function was given.

    match tokens with
    | [] -> errorMessage "" "" 0
    | next :: rest when expected_token = next -> rest
    | next :: _ -> errorMessage expected_token next 1


  let helperStartWith (pattern: string) (literal: string) =                                         // This helper function checks if a string starts with a given pattern.    
    literal.StartsWith(pattern) 


  let private matchIdentifier tokens =                                                              // This helper function matches an identifier. 

    let hd = accessHd tokens 

    match tokens with
    | [] -> errorMessage "" "" 0
    | next :: rest when helperStartWith "identifier:" next -> accessTl tokens []
    | next :: rest when helperStartWith "int_literal:" next -> accessTl tokens []
    | next :: rest when helperStartWith "real_literal:" next -> accessTl tokens []
    | next :: rest when helperStartWith "str_literal:" next -> accessTl tokens []
    | next :: _ -> errorMessage "identifier" next 1


  let private vardecl (tokens, symboltable) =

    match tokens with
    | "int" :: _ -> 
        
        if List.isEmpty tokens then
            errorMessage "int" (List.head tokens) 0
            (tokens, symboltable)
        
        else
            let T2 = matchToken "int" tokens
            let hd1 = accessHd tokens
            let tl = accessTl tokens []
            let hd2 = accessHd tl
            let len = stringLength "identifier:" 0
            let id = hd2.Substring(len)
            
            if contains symboltable id then
                errorMessage id "" 2
            
            else
                let T3 = matchIdentifier T2
                let T4 = matchToken ";" T3
                let symboltable2 = (id, hd1) :: symboltable
                (T4, symboltable2)

    | "real" :: _ -> 
        
        if List.isEmpty tokens then
            errorMessage "real" (List.head tokens) 0
            (tokens, symboltable)
        
        else
            let T2 = matchToken "real" tokens
            let hd1 = accessHd tokens
            let tl = List.tail tokens
            let tl = accessTl tokens ""
            let hd2 = accessHd tl
            let len = stringLength "identifier:" 0
            let id = hd2.Substring(len)
        
            if contains symboltable id then
                errorMessage id "" 2
        
            else
                let T3 = matchIdentifier T2
                let T4 = matchToken ";" T3
                let symboltable2 = (id, hd1) :: symboltable
                (T4, symboltable2)

    | _ -> (tokens, symboltable)


  let private helperInput (tokens, symboltable) =                                                   // This function parses and handles input statements.

    let hd1 = accessHd tokens
    let tl1 = List.tail tokens
    let tl2 = List.tail tl1
    let hd2 = accessHd tl2

    if string(hd1).StartsWith("identifier:") then
      let len = stringLength "identifier:" 0
      let id = hd2.Substring(len)
      
      if not (List.exists (fun (n, _) -> n = id) symboltable) then
        errorMessage id "" 3
    
    if string(hd2).StartsWith("identifier:") then
      let len2 = stringLength "identifier:" 0
      let id2 = hd2.Substring(len2)

      if not (List.exists (fun (n, _) -> n = id2) symboltable) then
        errorMessage id2 "" 3
    
    let T2 = matchToken "cin" tokens
    let T3 = matchToken ">>" T2
    let T4 = matchIdentifier T3
    let T5 = matchToken ";" T4

    (T5, symboltable)


  let private exprValue (tokens, symboltable) =                                                                    // This helper function parses and handles values of expressions.

    let hd1 = accessHd tokens

    if string(hd1).StartsWith("identifier:") then
      let len = stringLength "identifier:" 0
      let id = hd1.Substring(len)
      
      if not (List.exists (fun (n, _) -> n = id) symboltable) then
        errorMessage id "" 3
  
    match tokens with
    | hd :: _ when (helperStartWith "identifier:" hd) -> matchIdentifier tokens
    | hd :: _ when (helperStartWith "int_literal:" hd) -> matchIdentifier tokens
    | hd :: _ when (helperStartWith "real_literal:" hd) -> matchIdentifier tokens
    | hd :: _ when (helperStartWith "str_literal:" hd) -> matchIdentifier tokens
    | "true" :: _ -> matchToken "true" tokens                                                               
    | "false" :: _ -> matchToken "false" tokens
    | next :: _ -> errorMessage "identifier or literal" next 1
    | _ -> tokens


  let private exprOp (tokens, symboltable) =                                                                       // This helper function parses and handles expressions that are operators.
    
    match tokens with
    | "+" :: _ | "-" :: _ | "*" :: _ | "/" :: _ | "^" :: _ | "<" :: _ | "<=" :: _ | ">" :: _ | ">=" :: _ | "==" :: _ | "!=" :: _ -> 
        let tokens = matchToken (List.head tokens) tokens
        exprValue (tokens, symboltable)
    | _ -> tokens


  let private expr tokens symboltable =                                                                        // This function parses and handles complete expressions.
    
    let hd1 = accessHd tokens
    
    if string(hd1).StartsWith("identifier:") then
    
      let len = stringLength "identifier:" 0
      let id = hd1.Substring(len)
      
      if not (List.exists (fun (n, _) -> n = id) symboltable) then
        errorMessage id "" 3
    
      else
        let tokens = exprValue (tokens, symboltable)
        exprOp (tokens, symboltable)
    
    else
        let tokens = exprValue (tokens, symboltable)
        exprOp(tokens, symboltable)
  

  let private outputVal (tokens, symboltable) =                                                     // This helper function parses and handles output values.
    
    match tokens with
    | "endl" :: _ -> matchToken "endl" tokens
    | _ -> expr tokens symboltable


  let private output (tokens, symboltable) =                                                        // This function parses and handles output statements. 
    let hd1 = accessHd tokens
    
    if string(hd1).StartsWith("identifier:") then
      let len = stringLength "identifier:" 0
      let id = hd1.Substring(len)
    
      if not (List.exists (fun (n, _) -> n = id) symboltable) then
        errorMessage id "" 3

    let T2 = matchToken "cout" tokens
    let T3 = matchToken "<<" T2
    let T4 = outputVal(T3, symboltable)
    let T5 = matchToken ";" T4

    (T5, symboltable)


  let private assignment (tokens, symboltable) = 
    let hd1 = accessHd tokens
    
    if string(hd1).StartsWith("identifier:") then
      let len = String.length "identifier:"
      let id = string(hd1).Substring(len)
    
      if not (List.exists (fun (n, _) -> n = id) symboltable) then
        errorMessage id "" 3
    
      else
        let T2 = matchIdentifier tokens 
        let T3 = matchToken "=" T2
        let T4 = expr T3 symboltable
        let T5  = matchToken ";" T4
        (T5, symboltable)
    
    else
        let T2 = matchIdentifier tokens
        let T3 = matchToken "=" T2
        let T4 = expr T3 symboltable
        let T5  = matchToken ";" T4
        (T5, symboltable)


  let private helperColon (tokens, symboltable) =                                                  // This helper function parses and handles the colon symbol. 

    let T1 = matchToken ";" tokens 
    (T1, symboltable)


  let rec private stmt (tokens, symboltable) =
    
    match tokens with 
    | ";" :: tl ->   helperColon (tokens, symboltable)
    | "int" :: tl ->  vardecl (tokens, symboltable)
    | "real" :: tl ->  vardecl (tokens, symboltable)
    | "cin" :: tl -> helperInput (tokens, symboltable)
    | "cout" :: tl -> output (tokens, symboltable)                                           
    | "if" :: tl -> ifstmt (tokens, symboltable)
    | hd :: _ when helperStartWith "identifier:" hd -> assignment (tokens, symboltable)
    |  _ -> errorMessage "statement" "" 0


  and private helperIf (tokens, symboltable) =                                                                            // This helper function parses and handles the "if" statement.
    stmt (tokens, symboltable)


  and private helperElse (tokens, symboltable) =                                                                   // This helper function parses and handles the "else" statement.
    
    match tokens with
    | "else" :: tl ->
        let T2 = 
          matchToken "else" tokens
        stmt (T2, symboltable)
    | _ -> (tokens, symboltable)


  and ifstmt (tokens, symboltable) =                                                                // This function parses and handles "if" statements.
    tokens
    |> matchToken "if"  
    |> matchToken "("
    |> fun t -> expr t symboltable
    |> matchToken ")" 
    |> fun tokens -> helperIf (tokens, symboltable)
    |> fun (tokens, updatedSymbolTable1) -> (helperElse (tokens, updatedSymbolTable1))


  let rec private morestmts (tokens, symboltable) =
    match tokens with
    | "}" :: rest -> (tokens, symboltable)
    | _ -> 
        let (tokensAfterStmt, updatedSymbolTable) = stmt (tokens, symboltable)
        morestmts (tokensAfterStmt, updatedSymbolTable)


  let rec private stmts (tokens, symboltable) =
    
    let (rest_list, symboltable) = stmt (tokens, symboltable)
    let (rest_list2, symboltable) = morestmts (rest_list, symboltable)
    
    (rest_list2, symboltable)
  

  let private simpleC tokens = 
  
    tokens
    |> matchToken "void"
    |> matchToken "main"
    |> matchToken "("
    |> matchToken ")"
    |> matchToken "{"
    |> (fun x -> stmts (x, []))
    |> (fun (tokensAfterStmts, symboltable) -> 
        matchToken "}" tokensAfterStmts
        |> matchToken "$"
        |> (fun x -> (x, symboltable)))



  
  //
  // build_symboltable tokens
  //
  // Given a list of tokens, analyzes the program by looking
  // at variable declarations and collecting them into a
  // list. This list is known as a symbol table. Returns
  // a tuple (result, symboltable), where result is a string 
  // denoting "success" if valid, otherwise a string of the 
  // form "semantic_error:...".
  //
  // On success, the symboltable is a list of tuples of the
  // form (id, type), e.g. [("x","int"); ("y","real")]. On 
  // an error, the returned list is empty [].
  //
  let build_symboltable tokens = 
    try
      let (T2, symboltable) = simpleC tokens
      ("success", symboltable)
    with 
      | ex -> ("semantic_error: " + ex.Message, [])