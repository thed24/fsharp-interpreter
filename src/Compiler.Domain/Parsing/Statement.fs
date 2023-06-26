module Statement

open SyntaxErrors
open Expression
open Tokens

(*
program        → declaration* EOF ;

declaration    → varDecl
               | statement ;

statement      → exprStmt
               | printStmt ;
*)

// ---------- Types ------------
type ExpressionStatement = { Expression: Expression }
type PrintStatement = { Expression: Expression }
type VarStatement = { Name: Token; Initializer: Expression }

type Statement =
   | ExpressionStatement of ExpressionStatement
   | PrintStatement of PrintStatement
   | VarStatement of VarStatement
   
type StatementInput = { Tokens: Token list; Errors: SyntaxError list }

// ---------- Helpers ------------
let fakeStatement input tokens error =
    ExpressionStatement { Expression = LiteralExpr { Value = PrimaryType.Token { TokenType = FAKE; Lexeme = ""; Line = 0; Column = 0 } }}, { Tokens = tokens; Errors = error :: input.Errors }

let printStatement (input: StatementInput): Statement * StatementInput =
    let print, tokens = (List.tryHead input.Tokens, List.tail input.Tokens)
    let expression, tokens, errors = expression tokens |> fun (expression, output) -> (expression, output.Tokens, output.Errors)
    let semiColon, tokens = (List.tryHead tokens, List.tail tokens)

    match print, errors, semiColon with
    | Some _, [], Some semicolonToken when semicolonToken.TokenType = SEMICOLON -> Statement.PrintStatement { Expression = expression }, { Tokens = tokens; Errors = input.Errors }
    | Some _, [], _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected ';' after print statement."; })
    | _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected expression after print statement."; })

// ---------- Parsers ------------
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
        | Some _, Some nameToken, [], _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected ';' after variable declaration."; })
        | Some _, Some nameToken, _, _ -> fakeStatement input tokens (UnexpectedEndOfInput { Message = "Expected expression after variable declaration."; })
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
    let innerFn input =
        match peek input.Tokens [ PRINT; VAR; ] with
        | Some token when token.TokenType = PRINT -> printStatement input
        | Some token when token.TokenType = VAR -> varStatement input
        | _ -> expressionStatement input

    let result, input = innerFn input
    
    match peek input.Tokens [ EOF; ] with
    | Some token when token.TokenType = EOF -> result :: statements, input
    | _ -> statement input (result :: statements)