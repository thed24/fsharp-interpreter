module Expression

open SyntaxErrors
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

type PrimaryType<'a> =
    | Token of Token
    | Expression of 'a

type Binary<'a> = { Left: 'a; Operator: Token; Right: 'a }
type Unary<'a> = { Operator: Token; Right: 'a }
type Primary<'a> = { Value: PrimaryType<'a> }

type Expression =
    | BinaryExpr of Binary<Expression>
    | UnaryExpr of Unary<Expression>
    | LiteralExpr of Primary<Expression>
    
type PrimaryValue =
    | Number of float
    | String of string
    | Boolean of bool
    | Nil
    | Expression of Expression
    | Identifier of string

type ExpressionInput = { Tokens: Token list; Errors: SyntaxError list }

// ---------- Helpers ------------
let peek (tokens: Token list) (expectations: TokenType list) : Token option =
    match tokens with
    | [] -> None
    | head :: _ -> if expectations |> List.exists (fun expectation -> expectation = head.TokenType) then Some head else None

let fakeExpression (input: ExpressionInput) (error: SyntaxError) : Expression * ExpressionInput =
    LiteralExpr { Value = PrimaryType.Token { TokenType = FAKE; Lexeme = ""; Line = 0; Column = 0 } }, { Tokens = input.Tokens; Errors = error :: input.Errors }

// ---------- Parsers ------------
let rec primary (input: ExpressionInput) : Expression * ExpressionInput =
    let token = peek input.Tokens [ NUMBER; STRING; TRUE; FALSE; NIL; IDENTIFIER ]

    match token with
    | Some token -> LiteralExpr { Value = PrimaryType.Token token }, { Tokens = List.tail input.Tokens; Errors = input.Errors }
    | None ->
        let maybeToken = input.Tokens |> List.tryHead

        match maybeToken with
        | None -> fakeExpression input (UnexpectedEndOfInput { Message = "Unexpected end of input" })
        | Some token -> fakeExpression input (MissingRightOperand { Line = token.Line; Column = token.Column; Operator = token })

let rec unary (input: ExpressionInput) : Expression * ExpressionInput =
    let operator = peek input.Tokens [ MINUS; BANG ]

    match operator with
    | None -> primary input
    | Some operator ->
        let input' = { Tokens = List.tail input.Tokens; Errors = input.Errors }
        let rightExpr, updatedInput = unary input'

        (UnaryExpr { Operator = operator; Right = rightExpr }, updatedInput)

let rec binary (input: ExpressionInput) (expectations: TokenType list) (next: ExpressionInput -> Expression * ExpressionInput) : Expression * ExpressionInput =
    let rec binaryInner left input =
        let operator = peek input.Tokens expectations

        match operator with
        | None -> left, input
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

// ----- Strings -----
let rec expressionToString (expr: Expression) : string =
    match expr with
    | LiteralExpr primary -> primaryExpressionToString primary
    | UnaryExpr unary -> unaryExpressionToString unary
    | BinaryExpr binary -> binaryExpressionToString binary

and primaryExpressionToString (primary: Primary<Expression>) : string =
    match primary.Value with
        | PrimaryType.Token token -> token.Lexeme
        | PrimaryType.Expression expr -> expressionToString expr

and unaryExpressionToString (unary: Unary<Expression>) : string =
    match unary.Operator.TokenType with
    | MINUS -> "-(" + expressionToString unary.Right + ")"
    | BANG -> "!(" + expressionToString unary.Right + ")"
    | _ -> failwith "Unsupported unary operator"

and binaryExpressionToString (binary: Binary<Expression>) : string =
    let left = expressionToString binary.Left
    let operator = binary.Operator.Lexeme
    let right = expressionToString binary.Right
    "(" + left + " " + operator + " " + right + ")"

and errorExpressionToString (error: SyntaxError) : string =
    match error with
    | UnexpectedEndOfInput err -> err.Message
    | MissingRightOperand err -> $"Missing right operand for operator '%s{err.Operator.Lexeme}' at line %d{err.Line}, column %d{err.Column}"
