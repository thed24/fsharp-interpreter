module ExpressionEvaluation

open Evaluation
open Tokens

type ExpressionResult = 
    | Success of PrimaryValue
    | Error of string
    
let isTruthy (value: ExpressionResult) =
    match value with
    | Success (PrimaryValue.Boolean false) -> false
    | Success PrimaryValue.Nil -> false
    | _ -> true
    
let isTruthyVal (value: PrimaryValue) =
    match value with
    | PrimaryValue.Boolean false -> false
    | PrimaryValue.Nil -> false
    | _ -> true

let matchLiteral (literal: Primary<Expression>) (context: EvaluationContext) (evaluate: Expression -> EvaluationContext -> ExpressionResult): ExpressionResult =
    match literal.Value with
    | PrimaryType.Expression expr -> evaluate expr context
    | PrimaryType.Token token ->
        match token.TokenType with
        | STRING -> Success (PrimaryValue.String token.Lexeme)
        | NUMBER -> Success (PrimaryValue.Number (float token.Lexeme))
        | TRUE -> Success (PrimaryValue.Boolean true)
        | FALSE -> Success (PrimaryValue.Boolean false)
        | NIL -> Success PrimaryValue.Nil
        | IDENTIFIER ->
            let value = context.Find token.Lexeme
            match value with
            | Some value -> Success value
            | None -> Error $"Undefined variable '{token.Lexeme}'"
        | _ -> Error "Invalid literal"
    
let matchUnary (unary: Unary<Expression>) (context: EvaluationContext) (evaluate: Expression -> EvaluationContext -> ExpressionResult): ExpressionResult =
    match unary.Operator.TokenType with
    | BANG ->
        let right = evaluate unary.Right context
        Success (PrimaryValue.Boolean (not (isTruthy right)))
    | MINUS ->
        let right = evaluate unary.Right context
        match right with
        | Success (PrimaryValue.Number num) -> Success (PrimaryValue.Number (- num))
        | Error error -> Error error
        | _ -> Error "Invalid unary operator"
    | _ -> Error "Invalid unary operator"
    
let matchBinary (binary: Binary<Expression>) (context: EvaluationContext) (evaluate: Expression -> EvaluationContext -> ExpressionResult): ExpressionResult =
    match binary.Operator.TokenType with
    | PLUS ->
        let left = evaluate binary.Left context
        let right = evaluate binary.Right context
        match left, right with
        | Success (PrimaryValue.String left), Success (PrimaryValue.String right) -> Success (PrimaryValue.String (left + right))
        | Success (PrimaryValue.Number left), Success (PrimaryValue.Number right) -> Success (PrimaryValue.Number (left + right))
        | Error error, _ -> Error error
        | _, Error error -> Error error
        | _ -> Error "Invalid binary operator"
    | MINUS ->
        let left = evaluate binary.Left context
        let right = evaluate binary.Right context
        match left, right with
        | Success (PrimaryValue.Number left), Success (PrimaryValue.Number right) -> Success (PrimaryValue.Number (left - right))
        | Error error, _ -> Error error
        | _, Error error -> Error error
        | _ -> Error "Invalid binary operator"
    | STAR ->
        let left = evaluate binary.Left context
        let right = evaluate binary.Right context
        match left, right with
        | Success (PrimaryValue.Number left), Success (PrimaryValue.Number right) -> Success (PrimaryValue.Number (left * right))
        | Error error, _ -> Error error
        | _, Error error -> Error error
        | _ -> Error "Invalid binary operator"
    | SLASH ->
        let left = evaluate binary.Left context
        let right = evaluate binary.Right context
        match left, right with
        | Success (PrimaryValue.Number left), Success (PrimaryValue.Number right) -> Success (PrimaryValue.Number (left / right))
        | Error error, _ -> Error error
        | _, Error error -> Error error
        | _ -> Error "Invalid binary operator"
    | GREATER ->
        let left = evaluate binary.Left context
        let right = evaluate binary.Right context
        match left, right with
        | Success (PrimaryValue.Number left), Success (PrimaryValue.Number right) -> Success (PrimaryValue.Boolean (left > right))
        | Error error, _ -> Error error
        | _, Error error -> Error error
        | _ -> Error "Invalid binary operator"
    | GREATER_EQUAL ->
        let left = evaluate binary.Left context
        let right = evaluate binary.Right context
        match left, right with
        | Success (PrimaryValue.Number left), Success (PrimaryValue.Number right) -> Success (PrimaryValue.Boolean (left >= right))
        | Error error, _ -> Error error
        | _, Error error -> Error error
        | _ -> Error "Invalid binary operator"
    | LESS ->
        let left = evaluate binary.Left context
        let right = evaluate binary.Right context
        match left, right with
        | Success (PrimaryValue.Number left), Success (PrimaryValue.Number right) -> Success (PrimaryValue.Boolean (left < right))
        | Error error, _ -> Error error
        | _, Error error -> Error error
        | _ -> Error "Invalid binary operator"
    | LESS_EQUAL ->
        let left = evaluate binary.Left context
        let right = evaluate binary.Right context
        match left, right with
        | Success (PrimaryValue.Number left), Success (PrimaryValue.Number right) -> Success (PrimaryValue.Boolean (left <= right))
        | Error error, _ -> Error error
        | _, Error error -> Error error
        | _ -> Error "Invalid binary operator"
    | EQUAL_EQUAL ->
        let left = evaluate binary.Left context
        let right = evaluate binary.Right context
        Success (PrimaryValue.Boolean (left = right))
    | BANG_EQUAL ->
        let left = evaluate binary.Left context
        let right = evaluate binary.Right context
        Success (PrimaryValue.Boolean (left <> right))
    | OR ->
        let left = evaluate binary.Left context
        let right = evaluate binary.Right context
        match left, right with
        | Success left, Success right -> Success (PrimaryValue.Boolean (isTruthyVal left || isTruthyVal right))
        | Error error, _ -> Error error
        | _, Error error -> Error error
    | AND ->
        let left = evaluate binary.Left context
        let right = evaluate binary.Right context
        match left, right with
        | Success left, Success right -> Success (PrimaryValue.Boolean (isTruthyVal left && isTruthyVal right))
        | Error error, _ -> Error error
        | _, Error error -> Error error
    | _ -> Error "Invalid binary operator"

let rec evaluateExpression (expression: Expression) (context: EvaluationContext): ExpressionResult =
    match expression with
    | Expression.LiteralExpr expr -> matchLiteral expr context evaluateExpression
    | Expression.UnaryExpr expr -> matchUnary expr context evaluateExpression
    | Expression.BinaryExpr expr -> matchBinary expr context evaluateExpression
    