module Expression

open Errors
open Evaluation
open Tokens

(*    
    expression     → equality ;
    logic_or       → logic_and ( "or" logic_and )* ;
    logic_and      → equality ( "and" equality )* ;
    equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    term           → factor ( ( "-" | "+" ) factor )* ;
    factor         → unary ( ( "/" | "*" ) unary )* ;
    unary          → ( "!" | "-" ) unary
                    | primary ;
    primary        → NUMBER | STRING | "true" | "false" | "nil"
                    | "(" expression ")" ;
 *)

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

let rec logicAnd (input: ExpressionInput) : Expression * ExpressionInput =
    binary input [ AND ] equality

let rec logicOr (input: ExpressionInput) : Expression * ExpressionInput =
    binary input [ OR ] logicAnd

let expression (tokens: Token list) : Expression * ExpressionInput = 
    logicOr { Tokens = tokens; Errors = [] }
