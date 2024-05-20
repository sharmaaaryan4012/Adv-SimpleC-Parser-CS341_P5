namespace compiler

module checker =
  //
  // NOTE: all functions in the module must be indented.
  //

  let name_variable (identifierString: string) =
    let parts = identifierString.Split(':')
    parts.[1]
  let Type_variable (varName: string) (table: (string * string) list) = 
    let (_, varType) = List.find (fun (x, _) -> x = varName) table
    varType

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


  // this function checks the line of code for initialization line and compare each and every token for it  
  let  private vardecl tokens symboltable = 
    match tokens with
    | "int" :: _ -> 
                    let tokens2 = matchToken "int" tokens
                    let tokens3 = matchIdentifier tokens2                                                             // calls the matchIdentifier function for checking the Identifier 
                    matchToken ";" tokens3
                    
    | "real" :: _ -> 
                    let tokens2 = matchToken "real" tokens
                    let tokens3 = matchIdentifier tokens2                                                             // calls the matchIdentifier function for checking the Identifier 
                    matchToken ";" tokens3
    | _ -> tokens


  // this function checks the line of code for inputting something from the user
  let private input_func tokens symboltable = 
    let tokens2 = matchToken "cin" tokens
    let tokens3 = matchToken ">>" tokens2                                                             // compares the keyword ">>" with the code line token by using the function matchToken
    let tokens4 = matchIdentifier tokens3
    matchToken ";" tokens4



  let isExprOpr token =
      token = "+" // Check if token is "+"
      || token = "-" // Check if token is "-"
      || token = "*" // Check if token is "*"
      || token = "/" // Check if token is "/"
      || token = "^" // Check if token is "^"
      || token = "<" // Check if token is "<"
      || token = "<=" // Check if token is "<="
      || token = ">" // Check if token is ">"
      || token = ">=" // Check if token is ">="
      || token = "==" // Check if token is "=="
      || token = "!=" // Check if token is "!="

  let isArithmeticOpr (token: string) =
    token = "+" // Check if token is "+"
    || token = "-" // Check if token is "-"
    || token = "*" // Check if token is "*"
    || token = "/" // Check if token is "/"
    || token = "^" // Check if token is "^"

  let isComparatorOpr (token: string) =
    token = "<" // Check if token is "<"
    || token = "<=" // Check if token is "<="
    || token = ">" // Check if token is ">"
    || token = ">=" // Check if token is ">="
    || token = "==" // Check if token is "=="
    || token = "!=" // Check if token is "!="


  // this function checks the expression line for identifier, int_literal and str_literal and others by using the appropriate function 
  let private exprValue tokens symboltable=
    let head1 = List.head tokens
    match tokens with
    | hd :: _ when (beginswith "identifier:" hd) -> 
        let parts = head1.Split(':')
        let varName = parts.[1]
        let varType = Type_variable varName symboltable
        (matchIdentifier tokens, varType)
    | hd :: _ when (beginswith "int_literal:" hd) -> (matchIdentifier  tokens, "int")
    | hd :: _ when (beginswith "real_literal:" hd) -> (matchIdentifier tokens, "real")
    | hd :: _ when (beginswith "str_literal:" hd) -> (matchIdentifier tokens, "str")
    | "true" :: _ -> (matchToken "true" tokens, "bool")                                                              // compares the token with true keeyword if matches returns the list 
    | "false" :: _ -> (matchToken "false" tokens, "bool")
    | next_token :: _ -> failwith ("expecting identifier or literal, but found " + next_token)
    | _ -> (tokens, "")


  // this function compares the token with the different types of keyword
  let private exprOp tokens symboltable =
    let token_head = List.head tokens
    match tokens with
    | "+" :: _ | "-" :: _ | "*" :: _ | "/" :: _ | "^" :: _ | "<" :: _ | "<=" :: _ | ">" :: _ | ">=" :: _ | "==" :: _ | "!=" :: _ -> 
         matchToken (List.head tokens) tokens
        
    | _ -> failwith("expecting expression operator, but found " + token_head)


  // this function takes the expression and calls the functions exprValue and then exprOp by passing the tail of the list 
  let private expr tokens symboltable =
    let (tokens1, Type_variable1 )  = exprValue tokens symboltable
    let token2 = List.head tokens1

    if (isExprOpr token2) then
      let t3 = exprOp tokens1 symboltable


      let (tokens2, Type_variable2) = exprValue  t3 symboltable

      if (token2 = "==") && (Type_variable1 = "real" || Type_variable2 = "real") then
        printfn "warning: comparing real numbers with == may never be true"


      if (isArithmeticOpr token2) && not ((Type_variable1 = "int" || Type_variable2 = "real") && (Type_variable1.Equals Type_variable2)) then
        failwith ("operator " + token2 + " must involve 'int' or 'real'")


      if (isComparatorOpr token2) && not (Type_variable1.Equals Type_variable2) && not ((Type_variable1 = "true" || Type_variable1 = "false")  && (Type_variable2 = "false" || Type_variable2 = "true")) then
        failwith("type mismatch '" + Type_variable1 + "' " + token2 + " '" + Type_variable2 + "'")
      if (isComparatorOpr token2) then
        (tokens2, "bool")
      else
        (tokens2, Type_variable1)
    else
      // Return remaining tokens
        (tokens1, Type_variable1)



  
    // let tokens = exprValue tokens symboltable
    // let tokens2 = exprOp tokens symboltable
    // exprValue tokens2 symboltable


  // this function checks the token for endl, if not found it calls the expr function for the desired token check
  let private outputValue tokens symboltable =
    let nextToken = List.head tokens
    // Check if the next token is "endl"
    // Else, check if expr_value
    if nextToken = "endl" then
      matchToken "endl" tokens
    else
      let (T2, varType) = exprValue tokens symboltable
      T2


  // this function checks the line for outputting it checks each token by calling matchToken and then passes the tail to a different token check 
  let private output tokens symboltable = 
    let tokens2 = matchToken "cout" tokens
    let tokens3 = matchToken "<<" tokens2
    let tokens4 = outputValue tokens3 symboltable
    matchToken ";" tokens4
  

  // the function checks for assignment line, again it checkseach token by match token and passes the tail to the different token check call
  let private assignment tokens symboltable = 

    let head1 = List.head tokens
    let nameofvar = name_variable head1
    let typeofvar = Type_variable nameofvar symboltable

    let tokens2 = matchIdentifier tokens
    let tokens3 = matchToken "=" tokens2
    let (tokens4, typeofexpr)  = expr tokens3 symboltable

    if not (typeofvar.Equals typeofexpr) && not (typeofexpr = "int" && typeofvar = "real") then
      failwith("cannot assign '" + typeofexpr + "' to variable of type '" + typeofvar + "'")
    
    matchToken ";" tokens4
    

  // thsi function defines which line of code we are checking by matching the first token of the line and then calls the desired function 
  let rec private stmt tokens symboltable = 
      match tokens with 

      | ";" :: tl -> matchToken ";" tokens
      | "int" :: tl ->  vardecl tokens symboltable
      | "real" :: tl ->  vardecl tokens symboltable
      | "cin" :: tl -> input_func tokens symboltable
      | "cout" :: tl -> output tokens symboltable                                                // checks if its a cout line then calls the function which chekcs for the cout line
      | "if" :: tl -> ifstmt tokens symboltable
      | hd :: _ when beginswith "identifier:" hd -> assignment tokens symboltable
      | next_token :: _ -> failwith ("expecting statement, but found " + next_token) 
      |  _ -> tokens
      // if nothing matches then it calls the failwith function and shows the error

  // using the and operator instead of let to do mutual recursive calls
  and private then_part tokens symboltable = 
    stmt tokens symboltable

  // this function checks for the else part of the if statement 
  and private else_part tokens symboltable =
    let next_token = List.head tokens

    if next_token = "else" then
      let T2 = matchToken "else" tokens                                               // checks the else keyword, if matches then returns the tail
      stmt T2 symboltable
    else
      tokens


  // this function checks for each token for the if line by calling the desired function 
  and ifstmt tokens symboltable = 
    let tokens2 = matchToken "if" tokens
    let tokens3 = matchToken "(" tokens2
    let (tokens4, con_type) = expr tokens3 symboltable
    if not (con_type = "bool") then
      failwith("if condition must be 'bool', but found '" + con_type + "'")                                      // calls the expr function and checks the whole expression in the if statement
    let tokens5 = matchToken ")" tokens4
    let tokens6 = then_part tokens5 symboltable
    let tokens7 = else_part tokens6 symboltable
    tokens7                                                           // calling the tokens7 function which automatically calls every small function


  // this function is called for the other statement which also checks for the end of statement 
  let rec private morestmts tokens symboltable =
    match tokens with
    | "}" :: _ -> tokens                                                // case of the statements are finished 
    | _ -> 
        let tokensAfterStmt = stmt tokens symboltable
        morestmts tokensAfterStmt symboltable


  // this functions calls for each line and passes the other code in morestmts function which agains calls stmt function
  let rec private stmts tokens symboltable =
    let rest_list = stmt tokens symboltable
    let rest_list2 = morestmts rest_list symboltable
    rest_list2

  // this function is checking the basic outline like void function, parenthesis and curly braces
  let private simpleC tokens symboltable = 

      let T2 = matchToken "void" tokens
      let T3 = matchToken "main" T2
      let T4 = matchToken "(" T3
      let T5 = matchToken ")" T4
      let T6 = matchToken "{" T5
      let T7 = stmts T6 symboltable
      let T8 = matchToken "}" T7
      let T9 = matchToken "$" T8 // $ => EOF, there should be no more tokens
      T9




  //
  // typecheck tokens symboltable
  //
  // Given a list of tokens and a symbol table, type-checks 
  // the program to ensure program's variables and expressions
  // are type-compatible. If the program is valid, returns 
  // the string "success". If the program contains a semantic
  // error or warning, returns a string of the form
  // "type_error: ...".
  //
  let typecheck tokens symboltable = 
    try
      let T2 = simpleC tokens symboltable
      "success"
    with 
      | ex -> "type_error: " + ex.Message
