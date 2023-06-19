module Syntax

open Tokens

(*
    expression     → equality ;
    equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    term           → factor ( ( "-" | "+" ) factor )* ;
    factor         → unary ( ( "/" | "*" ) unary )* ;
    unary          → ( "!" | "-" ) unary
                    | primary ;
    primary        → NUMBER | STRING | "true" | "false" | "nil"
                    | "(" expression ")" ;
 *)

type Binary<'a> = { Left: 'a; Operator: Token; Right: 'a }
type Unary<'a> = { Operator: Token; Right: 'a }
type Primary = { Value: Token }

type Expression =
    | Expression of Expression
    | BinaryExpr of Binary<Expression>
    | UnaryExpr of Unary<Expression>
    | LiteralExpr of Primary

type UnexpectedEndOfInputError = { Message: string }
type MissingRightOperandError = { Line: int; Column: int; Operator: Token }

type Error =
    | UnexpectedEndOfInput of UnexpectedEndOfInputError
    | MissingRightOperand of MissingRightOperandError

type ExpressionInput = { Tokens: Token list; Errors: Error list }

// ---------- Helpers ------------
let peek (input: ExpressionInput) (expectations: TokenType list) : Token option =
    match input.Tokens with
    | [] -> None
    | head :: _ -> if expectations |> List.exists (fun expectation -> expectation = head.TokenType) then Some head else None

let fakeExpression (input: ExpressionInput) (error: Error) : Expression * ExpressionInput =
    Expression(LiteralExpr { Value = { TokenType = FAKE; Lexeme = ""; Line = 0; Column = 0 } }), { Tokens = input.Tokens; Errors = error :: input.Errors }

// ---------- Parsers ------------
let rec primary (input: ExpressionInput) : Expression * ExpressionInput =
    let token = peek input [ NUMBER; STRING; TRUE; FALSE; NIL ]

    match token with
    | Some token -> Expression(LiteralExpr { Value = token }), { Tokens = List.tail input.Tokens; Errors = input.Errors }
    | None ->
        let maybeToken = input.Tokens |> List.tryHead

        match maybeToken with
        | None -> fakeExpression input (UnexpectedEndOfInput { Message = "Unexpected end of input" })
        | Some token -> fakeExpression input (MissingRightOperand { Line = token.Line; Column = token.Column; Operator = token })

let rec unary (input: ExpressionInput) : Expression * ExpressionInput =
    let operator = peek input [ MINUS; BANG ]

    match operator with
    | None -> primary input
    | Some operator ->
        let input' = { Tokens = List.tail input.Tokens; Errors = input.Errors }
        let rightExpr, updatedInput = unary input'

        (UnaryExpr { Operator = operator; Right = rightExpr }, updatedInput)

let rec binary (input: ExpressionInput) (expectations: TokenType list) (next: ExpressionInput -> Expression * ExpressionInput) : Expression * ExpressionInput =
    let rec binaryInner left input =
        let operator = peek input expectations

        match operator with
        | None -> Expression left, { Tokens = input.Tokens; Errors = input.Errors }
        | Some operator ->
            let input' = { Tokens = List.tail input.Tokens; Errors = input.Errors }
            let right, updatedInput = next input'
            let res = { Left = left; Operator = operator; Right = right }

            binaryInner (BinaryExpr res) updatedInput

    let leftExpr, updatedInput = next input
    binaryInner leftExpr updatedInput

let rec factor (input: ExpressionInput) : Expression * ExpressionInput = 
    binary input [ SLASH; STAR ] unary

let rec term (input: ExpressionInput) : Expression * ExpressionInput = 
    binary input [ MINUS; PLUS ] factor

let rec comparison (input: ExpressionInput) : Expression * ExpressionInput = 
    binary input [ GREATER; GREATER_EQUAL; LESS; LESS_EQUAL ] term

let rec equality (input: ExpressionInput) : Expression * ExpressionInput = 
    binary input [ BANG_EQUAL; EQUAL_EQUAL ] comparison

// ---------- Entry Point ------------
let expression (tokens: Token list) : Expression * ExpressionInput = 
    equality { Tokens = tokens; Errors = [] }

// ----- Pretty Printing -----
let rec prettyPrint (expr: Expression) : string =
    match expr with
    | Expression innerExpr -> prettyPrint innerExpr
    | LiteralExpr primary -> prettyPrintPrimary primary
    | UnaryExpr unary -> prettyPrintUnary unary
    | BinaryExpr binary -> prettyPrintBinary binary

and prettyPrintPrimary (primary: Primary) : string = primary.Value.Lexeme

and prettyPrintUnary (unary: Unary<Expression>) : string =
    match unary.Operator.TokenType with
    | MINUS -> "-(" + prettyPrint unary.Right + ")"
    | BANG -> "!(" + prettyPrint unary.Right + ")"
    | _ -> failwith "Unsupported unary operator"

and prettyPrintBinary (binary: Binary<Expression>) : string =
    let left = prettyPrint binary.Left
    let operator = binary.Operator.Lexeme
    let right = prettyPrint binary.Right
    "(" + left + " " + operator + " " + right + ")"

and prettyPrintError (error: Error) : string =
    match error with
    | UnexpectedEndOfInput err -> err.Message
    | MissingRightOperand err -> $"Missing right operand for operator '%s{err.Operator.Lexeme}' at line %d{err.Line}, column %d{err.Column}"

and prettyPrintErrors (errors: Error list) : string = errors |> List.map prettyPrintError |> String.concat "\n"
