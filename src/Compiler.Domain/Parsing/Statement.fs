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
*)

// ---------- Types ------------
type ExpressionStatement = { Expression: Expression }
type PrintStatement = { Expression: Expression }
type VarStatement = { Name: Token; Initializer: Expression }
type BlockStatement<'a> = { Statements: 'a list }
type IfStatement<'a> = { Condition: Expression; ThenBranch: 'a; ElseBranch: 'a option }

type Statement =
   | ExpressionStatement of ExpressionStatement
   | PrintStatement of PrintStatement
   | VarStatement of VarStatement
   | BlockStatement of BlockStatement<Statement>
   | IfStatement of IfStatement<Statement>
   
type StatementInput = { Tokens: Token list; Errors: SyntaxError list }

// ---------- Helpers ------------
let fakeStatement input tokens error =
    ExpressionStatement { Expression = LiteralExpr { Value = PrimaryType.Token { TokenType = FAKE; Lexeme = ""; Line = 0; Column = 0 } }}, { Tokens = tokens; Errors = error :: input.Errors }
    
// ---------- Parsers ------------
let ifStatement (input: StatementInput) statement: Statement * StatementInput =
    let parseBranch (tokens: Token list) = 
        let statement, output = statement { Tokens = tokens; Errors = []; }
        match output.Errors with
        | [] -> (Some statement, output.Tokens, [])
        | _ -> (None, output.Tokens, output.Errors)

    let ifToken, tokens = (List.tryHead input.Tokens, List.tail input.Tokens)
    let leftParen, tokens = (List.tryHead tokens, List.tail tokens)
    let condition, tokens, expressionErrors = expression tokens |> fun (expression, output) -> (expression, output.Tokens, output.Errors)
    let rightParen, tokens = (List.tryHead tokens, List.tail tokens)
    let thenBranch, tokens, thenErrors = parseBranch tokens
    let elseToken, tokens = (List.tryHead tokens, List.tail tokens)
    let elseBranch, tokens, elseErrors = 
        match elseToken with
        | Some token when token.TokenType = ELSE -> parseBranch tokens
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

let varStatement (input: StatementInput): Statement * StatementInput =
    let var, tokens = (List.tryHead input.Tokens, List.tail input.Tokens)
    let name, tokens = (List.tryHead tokens, List.tail tokens)
    let middleOperator, tokens = (peek tokens [ EQUAL; SEMICOLON; ], List.tail tokens)

    match middleOperator with
    | Some equalsToken when equalsToken.TokenType = EQUAL ->
        let expression, tokens, errors = expression tokens |> fun (expression, output) -> (expression, output.Tokens, output.Errors)
        let semiColon, tokens = (List.tryHead tokens, List.tail tokens)

        match var, name, errors, semiColon with
        | Some _, Some nameToken, [], Some semicolonToken when semicolonToken.TokenType = SEMICOLON -> Statement.VarStatement { Name = nameToken; Initializer = expression }, { Tokens = tokens; Errors = input.Errors }
        | Some _, Some _, [], _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected ';' after variable declaration."; })
        | Some _, Some _, _, _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected expression after variable declaration."; })
        | _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected variable name after 'var'."; })
    | Some equalsToken when equalsToken.TokenType = SEMICOLON ->
        match var, name with
        | Some _, Some nameToken -> Statement.VarStatement { Name = nameToken; Initializer = LiteralExpr { Value = PrimaryType.Token { TokenType = NIL; Lexeme = ""; Column = 0; Line = 0; } } }, { Tokens = tokens; Errors = [] }
        | _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected variable name after 'var'."; })
    | _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected '=' or ';' after a variable name."; })
    
let expressionStatement (input: StatementInput): Statement * StatementInput =
    let expression = expression input.Tokens
    
    match expression with
    | expression, { Tokens = tokens; Errors = errors } -> Statement.ExpressionStatement { Expression = expression }, { Tokens = tokens; Errors = errors }

let rec statement input statements =
    let rec innerFn input =
        match peek input.Tokens [ PRINT; VAR; LEFT_BRACE; IF ] with
        | Some token when token.TokenType = PRINT -> printStatement input
        | Some token when token.TokenType = VAR -> varStatement input
        | Some token when token.TokenType = LEFT_BRACE -> blockStatement input statement
        | Some token when token.TokenType = IF -> ifStatement input innerFn
        | _ -> expressionStatement input

    let result, input = innerFn input
    
    match input.Tokens, peek input.Tokens [ EOF; ] with
    | _, Some token when token.TokenType = EOF -> (statements @ [ result ]), input
    | [], _ -> (statements @ [ result ]), input
    | _ -> statement input (statements @ [ result ])
