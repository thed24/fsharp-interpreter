module Statement

open Errors
open Evaluation
open Expression
open Tokens

(*
program        → declaration* EOF ;

declaration    → varDecl
               | statement ;

varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;

statement      → exprStmt
               | printStmt
               | ifStmt
               | block ;

exprStmt       → expression ";" ;
printStmt      → "print" expression ";" ;
block          → "{" declaration* "}" ;
ifStmt         → "if" "(" expression ")" statement ( "else" statement )? ;
whileStmt      → "while" "(" expression ")" statement ;
forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
                           expression? ";"
                           expression? ")" statement ;
*)

// ---------- Types ------------
type ExpressionStatement = { Expression: Expression }
type PrintStatement = { Expression: Expression }
type VarStatement = { Name: Token; Initializer: Expression; RequiresExistingVariable: bool }
type BlockStatement<'a> = { Statements: 'a list }
type IfStatement<'a> = { Condition: Expression; ThenBranch: 'a; ElseBranch: 'a option }
type WhileStatement<'a> = { Condition: Expression; Body: 'a }

type Statement =
   | ExpressionStatement of ExpressionStatement
   | PrintStatement of PrintStatement
   | VarStatement of VarStatement
   | BlockStatement of BlockStatement<Statement>
   | IfStatement of IfStatement<Statement>
   | WhileStatement of WhileStatement<Statement>
   
type StatementInput = { Tokens: Token list; Errors: SyntaxError list }

// ---------- Helpers ------------
let fakeStatement input tokens error =
    ExpressionStatement { Expression = LiteralExpr { Value = PrimaryType.Token { TokenType = FAKE; Lexeme = ""; Line = 0; Column = 0 } }}, { Tokens = tokens; Errors = error :: input.Errors }

let parseBranch (tokens: Token list) statement = 
    let statement, output = statement { Tokens = tokens; Errors = []; }
    match output.Errors with
    | [] -> (Some statement, output.Tokens, [])
    | _ -> (None, output.Tokens, output.Errors)

// ---------- Parsers ------------
let whileStatement (input: StatementInput) statement: Statement * StatementInput =
    let whileToken, tokens = (List.tryHead input.Tokens, List.tail input.Tokens)
    let leftParen, tokens = (List.tryHead tokens, List.tail tokens)
    let condition, tokens, errors = expression tokens |> fun (expression, output) -> (expression, output.Tokens, output.Errors)
    let rightParen, tokens = (List.tryHead tokens, List.tail tokens)
    let body, tokens, bodyErrors = parseBranch tokens statement

    match whileToken, leftParen, rightParen, body, errors, bodyErrors with
    | Some _, Some _, Some _, Some body, [], [] -> Statement.WhileStatement { Condition = condition; Body = body }, { Tokens = tokens; Errors = input.Errors }
    | Some _, Some _, Some _, None, [], _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected statement after 'while' condition."; })
    | Some _, Some _, None, _, _, _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected ')' after 'while' condition."; })
    | Some _, None, _, _, _, _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected '(' after 'while'."; })
    | Some _, _, _, _, _, _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected expression after 'while'."; })
    | _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected 'while'."; })

let ifStatement (input: StatementInput) statement: Statement * StatementInput =
    let ifToken, tokens = (List.tryHead input.Tokens, List.tail input.Tokens)
    let leftParen, tokens = (List.tryHead tokens, List.tail tokens)
    let condition, tokens, expressionErrors = expression tokens |> fun (expression, output) -> (expression, output.Tokens, output.Errors)
    let rightParen, tokens = (List.tryHead tokens, List.tail tokens)
    let thenBranch, tokens, thenErrors = parseBranch tokens statement
    let elseToken, tokens = (List.tryHead tokens, List.tail tokens)
    let elseBranch, tokens, elseErrors = 
        match elseToken with
        | Some token when token.TokenType = ELSE -> parseBranch tokens statement
        | _ -> (None, tokens, [])
    
    match ifToken, leftParen, rightParen, thenBranch, elseBranch, expressionErrors, thenErrors, elseErrors with
    | Some _, Some _, Some _, Some thenBranch, Some elseBranch, [], [], [] -> Statement.IfStatement { Condition = condition; ThenBranch = thenBranch; ElseBranch = Some elseBranch }, { Tokens = tokens; Errors = input.Errors }
    | Some _, Some _, Some _, Some thenBranch, None, [], [], [] -> Statement.IfStatement { Condition = condition; ThenBranch = thenBranch; ElseBranch = None }, { Tokens = tokens; Errors = input.Errors }
    | Some _, Some _, Some _, None, None, [], [], [] -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected statement after 'if' condition."; })
    | Some _, Some _, None, _, _, _, _, _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected ')' after 'if' condition."; })
    | Some _, None, _, _, _, _, _, _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected '(' after 'if'."; })
    | Some _, _, _, _, _, _, _, _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected expression after 'if'."; })
    | _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected 'if'."; })

let printStatement (input: StatementInput): Statement * StatementInput =
    let print, tokens = (List.tryHead input.Tokens, List.tail input.Tokens)
    let expression, tokens, errors = expression tokens |> fun (expression, output) -> (expression, output.Tokens, output.Errors)
    let semiColon, tokens = (List.tryHead tokens, List.tail tokens)

    match print, errors, semiColon with
    | Some _, [], Some semicolonToken when semicolonToken.TokenType = SEMICOLON -> Statement.PrintStatement { Expression = expression }, { Tokens = tokens; Errors = input.Errors }
    | Some _, [], _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected ';' after print statement."; })
    | _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected expression after print statement."; })

let rec parseNestedBraces (tokens: Token list) (openBraces: int) (nestedTokens: Token list) =
    match tokens, openBraces with
    | [], _ -> ([], tokens)
    | token :: remaining, 0 when token.TokenType = RIGHT_BRACE -> (nestedTokens, remaining)
    | token :: remaining, _ ->
        match token.TokenType with
        | LEFT_BRACE -> parseNestedBraces remaining (openBraces + 1) (nestedTokens @ [token])
        | RIGHT_BRACE when openBraces = 1 -> (nestedTokens, remaining)
        | RIGHT_BRACE -> parseNestedBraces remaining (openBraces - 1) (nestedTokens @ [token])
        | _ -> parseNestedBraces remaining openBraces (nestedTokens @ [token])

let blockStatement (input: StatementInput) statement: Statement * StatementInput =
    let leftBrace, tokens = (List.tryHead input.Tokens, List.tail input.Tokens)
    let tokensForBlock, remainingTokens = parseNestedBraces tokens 1 []
    
    match leftBrace, tokensForBlock, remainingTokens with
    | Some _, [], _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected '{' before block statement."; })
    | Some _, _, [] -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected '}' after block statement."; })
    | Some _, _, _ -> 
        let statements, _ = statement { Tokens = tokensForBlock; Errors = []; } [] |> fun (statements, output) -> (statements, output.Errors)
        Statement.BlockStatement { Statements = statements }, { Tokens = remainingTokens; Errors = input.Errors }
    | _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected '{' before block statement."; })

let assignment name tokens input requiresExistingVariable =
    let expression, tokens, errors = expression tokens |> fun (expression, output) -> (expression, output.Tokens, output.Errors)
    let semiColon, tokens = (List.tryHead tokens, List.tail tokens)

    match name, errors, semiColon with
    | Some nameToken, [], Some semicolonToken when semicolonToken.TokenType = SEMICOLON -> Statement.VarStatement { Name = nameToken; Initializer = expression; RequiresExistingVariable = requiresExistingVariable }, { Tokens = tokens; Errors = input.Errors }
    | Some nameToken, [], Some semicolonToken when semicolonToken.TokenType = RIGHT_PAREN -> Statement.VarStatement { Name = nameToken; Initializer = expression; RequiresExistingVariable = requiresExistingVariable }, { Tokens = tokens; Errors = input.Errors }
    | Some _, [], _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected ';' after variable declaration."; })
    | Some _, _, _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected expression after variable declaration."; })
    | _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected variable name after 'var'."; })

let varStatement (input: StatementInput): Statement * StatementInput =
    let var, tokens = (List.tryHead input.Tokens, List.tail input.Tokens)
    let name, tokens = (List.tryHead tokens, List.tail tokens)
    let middleOperator, tokens = (peek tokens [ EQUAL; SEMICOLON; ], List.tail tokens)

    match middleOperator with
    | Some equalsToken when equalsToken.TokenType = EQUAL -> assignment name tokens input false
    | Some equalsToken when equalsToken.TokenType = SEMICOLON ->
        match var, name with
        | Some _, Some nameToken -> Statement.VarStatement { Name = nameToken; RequiresExistingVariable = false; Initializer = LiteralExpr { Value = PrimaryType.Token { TokenType = NIL; Lexeme = ""; Column = 0; Line = 0; } } }, { Tokens = tokens; Errors = [] }
        | _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected variable name after 'var'."; })
    | _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected '=' or ';' after a variable name."; })
    
let assignmentStatement (input: StatementInput): Statement * StatementInput =
    let name, tokens = (List.tryHead input.Tokens, List.tail input.Tokens)
    let middleOperator, tokens = (peek tokens [ EQUAL; SEMICOLON; ], List.tail tokens)

    match middleOperator with
    | Some equalsToken when equalsToken.TokenType = EQUAL -> assignment name tokens input true
    | Some equalsToken when equalsToken.TokenType = SEMICOLON ->
        match name with
        | Some nameToken -> Statement.VarStatement { Name = nameToken; RequiresExistingVariable = true; Initializer = LiteralExpr { Value = PrimaryType.Token { TokenType = NIL; Lexeme = ""; Column = 0; Line = 0; } } }, { Tokens = tokens; Errors = [] }
        | _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected variable name after 'var'."; })
    | _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected '=' or ';' after a variable name."; })
    
let expressionStatement (input: StatementInput): Statement * StatementInput =
    let expression = expression input.Tokens
    
    match expression with
    | expression, { Tokens = tokens; Errors = errors } -> Statement.ExpressionStatement { Expression = expression }, { Tokens = tokens; Errors = errors }

let forStatement (input: StatementInput) statement: Statement * StatementInput =
    let forToken, tokens = (List.tryHead input.Tokens, List.tail input.Tokens)
    let leftParen, tokens = (List.tryHead tokens, List.tail tokens)
    let initializer, tokens, initializerErrors = 
        match List.tryHead tokens with
        | Some token when token.TokenType = VAR -> varStatement { Tokens = tokens; Errors = [] } |> fun (statement, output) -> (Some statement, output.Tokens, output.Errors)
        | Some token when token.TokenType = SEMICOLON -> (None, tokens, [])
        | _ -> (None, tokens, [ UnexpectedEndOfInput { Message = "Expected 'var' or ';' after 'for'."; } ])
    let condition, tokens, conditionErrors =
        match List.tryHead tokens with
        | Some token when token.TokenType = SEMICOLON -> (None, tokens, [])
        | _ -> expression tokens |> fun (expression, output) -> (Some expression, output.Tokens, output.Errors)
    let semiColon, tokens = (List.tryHead tokens, List.tail tokens)
    let increment, tokens, incrementErrors = 
        match List.tryHead tokens with
        | Some token when token.TokenType = RIGHT_PAREN -> (None, tokens, [])
        | Some token when token.TokenType = SEMICOLON -> (None, tokens, [])
        | Some token when token.TokenType = VAR -> varStatement { Tokens = tokens; Errors = [] } |> fun (statement, output) -> (Some statement, output.Tokens, output.Errors)
        | Some token when token.TokenType = IDENTIFIER -> assignmentStatement { Tokens = tokens; Errors = [] } |> fun (statement, output) -> (Some statement, output.Tokens, output.Errors)
        | _ -> (None, tokens, [ UnexpectedEndOfInput { Message = "Expected 'var', identifier, or ';' after 'for'."; } ])
    let body, tokens, bodyErrors = parseBranch tokens statement
    
    match forToken, leftParen, initializer, condition, semiColon, increment, body with
    | Some _, Some _, _, _, _, _, _ when initializerErrors <> [] -> fakeStatement input tokens (List.tryHead initializerErrors |> Option.get)
    | Some _, Some _, _, _, _, _, _ when conditionErrors <> [] -> fakeStatement input tokens (List.tryHead conditionErrors |> Option.get)
    | Some _, Some _, _, _, _, _, _ when incrementErrors <> [] -> fakeStatement input tokens (List.tryHead incrementErrors |> Option.get)
    | Some _, Some _, _, _, _, _, _ when bodyErrors <> [] -> fakeStatement input tokens (List.tryHead bodyErrors |> Option.get)
    | Some _, Some _, _, _, _, _, _ ->
        let initializer = if initializer = None then Statement.ExpressionStatement { Expression = LiteralExpr { Value = PrimaryType.Token { TokenType = NIL; Lexeme = ""; Column = 0; Line = 0; } } } else Option.get initializer
        let condition = if condition = None then LiteralExpr { Value = PrimaryType.Token { TokenType = TRUE; Lexeme = ""; Column = 0; Line = 0; } } else Option.get condition
        let increment = if increment = None then Statement.ExpressionStatement { Expression = LiteralExpr { Value = PrimaryType.Token { TokenType = NIL; Lexeme = ""; Column = 0; Line = 0; } } } else Option.get increment
        let body = if body = None then Statement.ExpressionStatement { Expression = LiteralExpr { Value = PrimaryType.Token { TokenType = NIL; Lexeme = ""; Column = 0; Line = 0; } } } else Option.get body
        
        let whileStatement = Statement.WhileStatement { Condition = condition; Body = Statement.BlockStatement { Statements = [ body; increment ]; }; }
        let result = Statement.BlockStatement { Statements = [ initializer; whileStatement ]; }
        
        result, { Tokens = tokens; Errors = [] }
    | _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected ')' after 'for' clauses."; })

let rec statement input statements =
    let rec innerFn input =
        match peek input.Tokens [ PRINT; VAR; LEFT_BRACE; IF; WHILE; IDENTIFIER; FOR ] with
        | Some token when token.TokenType = PRINT -> printStatement input
        | Some token when token.TokenType = VAR -> varStatement input
        | Some token when token.TokenType = LEFT_BRACE -> blockStatement input statement
        | Some token when token.TokenType = IF -> ifStatement input innerFn
        | Some token when token.TokenType = WHILE -> whileStatement input innerFn
        | Some token when token.TokenType = IDENTIFIER -> assignmentStatement input
        | Some token when token.TokenType = FOR -> forStatement input innerFn
        | _ -> expressionStatement input

    let result, input = innerFn input
    
    match input.Tokens, peek input.Tokens [ EOF; ] with
    | _, Some token when token.TokenType = EOF -> (statements @ [ result ]), input
    | [], _ -> (statements @ [ result ]), input
    | _ -> statement input (statements @ [ result ])
