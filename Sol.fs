namespace compiler

module analyzer =




  // this function extracts the head of the list compares and it with required token and returns the list 
  let private matchToken expected_token tokens =
    let next_token = List.head tokens                                                              // saving the head of the list into next_token
    if expected_token = next_token then  
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)                       // returning with failwith if the desired token is not found 


  // this function checks if the string matches with the literal by using the Startswith function 
  let beginswith (pattern: string) (literal: string) =        
    literal.StartsWith (pattern) 


  // this function matches the head of the token for identfier, int literals and string literals, if it matches it returns the tail of the list 
  let private matchIdentifier tokens =
    match tokens with
    | [] -> failwith "Unexpected end of input"
    | next_token :: rest when (beginswith "identifier:" next_token ) -> rest
    | next_token :: rest when (beginswith "int_literal:" next_token ) -> rest
    | next_token :: rest when (beginswith "real_literal:" next_token ) -> rest
    | next_token :: rest when (beginswith "str_literal:" next_token ) -> rest
    | next_token :: _ -> failwith ("expecting identifier, but found " + next_token)                  // returning with failwith explaining what was required 



  let rec private list_contain symboltable name = 
    match symboltable with 
    | [] -> false
    | (hd1 , hd2) :: tl when name = hd1 -> true
    | (hd1 , hd2) :: tl -> list_contain tl name

  // this function checks the line of code for initialization line and compare each and every token for it  
  let  private vardecl (tokens, symboltable) = 
    match tokens with
    | "int" :: _ -> 
                    if tokens = [] then
                      (tokens, symboltable)
                    else
                      let tokens2 = matchToken "int" tokens
                      let head1 = List.head tokens 
                      let tail = List.tail tokens
                      let head2 = List.head tail
                      let strlen = String.length "identifier:"
                      let name = head2.Substring(strlen)
                      if list_contain symboltable name then
                        failwith ("redefinition of variable '" + name + "'")
                      else
                        let tokens3 = matchIdentifier tokens2                                                             // calls the matchIdentifier function for checking the Identifier 
                        let tokens4 = matchToken ";" tokens3
                        let symboltable2 = (name, head1) :: symboltable
                        (tokens4, symboltable2)
    | "real" :: _ -> 
                    if tokens = [] then
                      (tokens, symboltable)
                    else
                      let tokens2 = matchToken "real" tokens
                      let head1 = List.head tokens 
                      let tail = List.tail tokens
                      let head2 = List.head tail
                      let strlen = String.length "identifier:"
                      let name = head2.Substring(strlen)
                      if list_contain symboltable name then
                        failwith ("redefinition of variable '" + name + "'")
                      else
                        let tokens3 = matchIdentifier tokens2                                                             // calls the matchIdentifier function for checking the Identifier 
                        let tokens4 = matchToken ";" tokens3
                        let symboltable2 = (name, head1) :: symboltable
                        (tokens4, symboltable2)
    | _ -> (tokens, symboltable)



  

  let private input_func (tokens, symboltable) =
    let head1 = List.head tokens
    let tail1 = List.tail tokens
    let tail2 = List.tail tail1
    let head2 = List.head tail2
    if string(head1).StartsWith("identifier:") then
      let strlen = String.length "identifier:"
      let name = string(head1).Substring(strlen)
      if not (List.exists (fun (n, _) -> n = name) symboltable) then
        failwith ("variable '" + name + "' undefined")
    if string(head2).StartsWith("identifier:") then
      let strlen2 = String.length "identifier:"
      let name2 = string(head2).Substring(strlen2)
      if not (List.exists (fun (n, _) -> n = name2) symboltable) then
        failwith ("variable '" + name2 + "' undefined") 
    let tokens2 = matchToken "cin" tokens
    let tokens3 = matchToken ">>" tokens2                                                             // compares the keyword ">>" with the code line token by using the function matchToken
    let tokens4 = matchIdentifier tokens3
    let tokens5 = matchToken ";" tokens4
    (tokens5, symboltable)


  // this function checks the expression line for identifier, int_literal and str_literal and others by using the appropriate function 
  let private exprValue tokens symboltable =

    let head1 = List.head tokens
    if string(head1).StartsWith("identifier:") then
      let strlen = String.length "identifier:"
      let name = string(head1).Substring(strlen)
      if not (List.exists (fun (n, _) -> n = name) symboltable) then
        failwith ("variable '" + name + "' undefined")
    match tokens with
    | hd :: _ when (beginswith "identifier:" hd) -> matchIdentifier tokens
    | hd :: _ when (beginswith "int_literal:" hd) -> matchIdentifier tokens
    | hd :: _ when (beginswith "real_literal:" hd) -> matchIdentifier tokens
    | hd :: _ when (beginswith "str_literal:" hd) -> matchIdentifier tokens
    | "true" :: _ -> matchToken "true" tokens                                                               // compares the token with true keeyword if matches returns the list 
    | "false" :: _ -> matchToken "false" tokens
    | next_token :: _ -> failwith ("expecting identifier or literal, but found " + next_token)
    | _ -> tokens


  // this function compares the token with the different types of keyword
  let private exprOp tokens symboltable =
    match tokens with
    | "+" :: _ | "-" :: _ | "*" :: _ | "/" :: _ | "^" :: _ | "<" :: _ | "<=" :: _ | ">" :: _ | ">=" :: _ | "==" :: _ | "!=" :: _ -> 
        let tokens = matchToken (List.head tokens) tokens
        exprValue tokens symboltable
    | _ -> tokens


  // this function takes the expression and calls the functions exprValue and then exprOp by passing the tail of the list 
  let private expr tokens symboltable =
  
    let head1 = List.head tokens
    if string(head1).StartsWith("identifier:") then
      let strlen = String.length "identifier:"
      let name = string(head1).Substring(strlen)
      if not (List.exists (fun (n, _) -> n = name) symboltable) then
        failwith ("variable '" + name + "' undefined")
      else
        let tokens = exprValue tokens symboltable
        exprOp tokens symboltable
    else
        let tokens = exprValue tokens symboltable
        exprOp tokens symboltable



  // this function checks the token for endl, if not found it calls the expr function for the desired token check
  let private outputValue tokens symboltable =
    match tokens with
    | "endl" :: _ -> matchToken "endl" tokens
    | _ -> expr tokens symboltable



  // this function checks the line for outputting it checks each token by calling matchToken and then passes the tail to a different token check 
  let private output (tokens, symboltable) = 
    let head1 = List.head tokens
    if string(head1).StartsWith("identifier:") then
      let strlen = String.length "identifier:"
      let name = string(head1).Substring(strlen)
      if not (List.exists (fun (n, _) -> n = name) symboltable) then
        failwith ("variable '" + name + "' undefined")
    let tokens2 = matchToken "cout" tokens
    let tokens3 = matchToken "<<" tokens2
    let tokens4 = outputValue tokens3 symboltable
    let tokens5 = matchToken ";" tokens4
    (tokens5, symboltable)

  // the function checks for assignment line, again it checkseach token by match token and passes the tail to the different token check call
  let private assignment (tokens, symboltable) = 
    let head1 = List.head tokens
    if string(head1).StartsWith("identifier:") then
      let strlen = String.length "identifier:"
      let name = string(head1).Substring(strlen)
      if not (List.exists (fun (n, _) -> n = name) symboltable) then
        failwith ("variable '" + name + "' undefined")
      else
        let tokens2 = matchIdentifier tokens 
        let tokens3 = matchToken "=" tokens2
        let tokens4 = expr tokens3 symboltable
        let tokens5  = matchToken ";" tokens4
        (tokens5, symboltable)
    else
        let tokens2 = matchIdentifier tokens
        let tokens3 = matchToken "=" tokens2
        let tokens4 = expr tokens3 symboltable
        let tokens5  = matchToken ";" tokens4
        (tokens5, symboltable)

  let private colon_func (tokens, symboltable) = 
    let tokens1 = matchToken ";" tokens 
    (tokens1, symboltable)

  // thsi function defines which line of code we are checking by matching the first token of the line and then calls the desired function 
  let rec private stmt (tokens, symboltable) =
      match tokens with 
      | ";" :: tl ->   colon_func (tokens, symboltable)
      | "int" :: tl ->  vardecl (tokens, symboltable)
      | "real" :: tl ->  vardecl (tokens, symboltable)
      | "cin" :: tl -> input_func (tokens, symboltable)
      | "cout" :: tl -> output (tokens, symboltable)                                           // checks if its a cout line then calls the function which chekcs for the cout line
      | "if" :: tl -> ifstmt (tokens, symboltable)
      | hd :: _ when beginswith "identifier:" hd -> assignment (tokens, symboltable)
      //| next_token :: _ -> failwith ("expecting statement, but found " + next_token) 
      |  _ -> failwith ("expecting statement, but found ")
      // if nothing matches then it calls the failwith function and shows the error

  // using the and operator instead of let to do mutual recursive calls
  and private then_part (tokens, symboltable) = 
    stmt (tokens, symboltable)

  // this function checks for the else part of the if statement 
  and private else_part (tokens, symboltable) =
    //printfn "test5"
    let next_token = List.head tokens

    if next_token = "else" then
      let T2 = matchToken "else" tokens                                               // checks the else keyword, if matches then returns the tail
      stmt (T2, symboltable)
    else
      (tokens, symboltable) 


  // this function checks for each token for the if line by calling the desired function 
  and ifstmt (tokens, symboltable) = 
    // printfn "test4"
    let tokens2 = matchToken "if" tokens
    let tokens3 = matchToken "(" tokens2
    let tokens4 = expr tokens3 symboltable                                    // calls the expr function and checks the whole expression in the if statement
    let tokens5 = matchToken ")" tokens4
    let (tokens6, symboltable) = then_part (tokens5, symboltable)
    let (tokens7, symboltable) = else_part (tokens6, symboltable)
    (tokens7, symboltable)                                                          // calling the tokens7 function which automatically calls every small function


  // this function is called for the other statement which also checks for the end of statement 
  let rec private morestmts (tokens, symboltable) =
    // printfn "test3"
    match tokens with
    | "}" :: _ -> (tokens, symboltable)                                                // case of the statements are finished 
    | _ -> 
        let (tokensAfterStmt, symboltable) = stmt (tokens, symboltable)
        morestmts (tokensAfterStmt, symboltable)


  // this functions calls for each line and passes the other code in morestmts function which agains calls stmt function
  let rec private stmts (tokens, symboltable) =
    let (rest_list, symboltable) = stmt (tokens, symboltable)
    let (rest_list2, symboltable) = morestmts (rest_list, symboltable)
    (rest_list2, symboltable)

  // this function is checking the basic outline like void function, parenthesis and curly braces
  let private simpleC tokens = 
      let T2 = matchToken "void" tokens
      let T3 = matchToken "main" T2
      let T4 = matchToken "(" T3
      let T5 = matchToken ")" T4
      let T6 = matchToken "{" T5
      let (T7, symboltable) = stmts (T6, [])
      let T8 = matchToken "}" T7
      let T9 = matchToken "$" T8 // $ => EOF, there should be no more tokens
      (T9, symboltable)

  // this function uses the try and catch function which either displays success or the error
  let parse tokens = 
    try
      let result = simpleC tokens
      "Success!"
    with 
      | ex -> "syntax_error: " + ex.Message

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
  // form (name, type), e.g. [("x","int"); ("y","real")]. On 
  // an error, the returned list is empty [].
  //
  let build_symboltable tokens = 
    try
      let (T2, symboltable) = simpleC tokens
      ("success", symboltable)
    with 
      | ex -> ("semantic_error: " + ex.Message, [])