module Statement

open SyntaxErrors
open Expression
open Tokens

(*
    program        → statement* EOF ;

    statement      → exprStmt
                   | printStmt ;

    exprStmt       → expression ";" ;
    printStmt      → "print" expression ";" ;
*)

type ExpressionStatement = { Expression: Expression }
type PrintStatement = { Expression: Expression }

type Statement =
   | ExpressionStatement of ExpressionStatement
   | PrintStatement of PrintStatement
   
type StatementInput = { Tokens: Token list; Errors: SyntaxError list }

let fakeStatement input error =
    ExpressionStatement { Expression = LiteralExpr { Value = PrimaryType.Token { TokenType = FAKE; Lexeme = ""; Line = 0; Column = 0 } }}, { Tokens = input.Tokens; Errors = error :: input.Errors }

let printStatement (input: StatementInput): Statement * StatementInput =
    let token = List.tryHead input.Tokens
    let expression = expression (List.tail input.Tokens)
    
    match token, expression with
    | Some token, (expression, _) -> Statement.PrintStatement { Expression = expression }, { Tokens = input.Tokens; Errors = input.Errors }
    | _ -> fakeStatement input (UnexpectedEndOfInput { Message = "Expected expression after print statement."; })

let expressionStatement (input: StatementInput): Statement * StatementInput =
    let expression = expression input.Tokens
    
    match expression with
    | expression, { Tokens = tokens; Errors = errors } -> Statement.ExpressionStatement { Expression = expression }, { Tokens = tokens; Errors = errors }

let rec statement tokens =
    let input = { Tokens = tokens; Errors = [] }
        
    let consume input =
        match peek input.Tokens [ PRINT; ] with
        | Some _ -> printStatement input
        | _ -> expressionStatement input

    consume input
