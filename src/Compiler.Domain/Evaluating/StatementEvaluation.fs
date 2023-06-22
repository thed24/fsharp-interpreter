module StatementEvaluation

open Expression
open Statement

type StatementValue =
    | PrintStatement of PrimaryValue
    | ExpressionStatement of PrimaryValue

let evaluateExpressionStatement (statement: ExpressionStatement): string option =
    let evaluatedExpression = ExpressionEvaluation.evaluateExpression statement.Expression
    match evaluatedExpression with
    | ExpressionEvaluation.Success value -> None
    | _ -> Some "Expression statement must be followed by an expression"

let evaluatePrintStatement (statement: PrintStatement): string option =
    let evaluatedExpression = ExpressionEvaluation.evaluateExpression statement.Expression
    match evaluatedExpression with
    | ExpressionEvaluation.Success value ->
        match value with
        | PrimaryValue.String str ->
            printfn "%s" str
            None
        | PrimaryValue.Number int ->
            printfn "%f" int
            None
        | PrimaryValue.Boolean bool ->
            printfn "%b" bool
            None
        | PrimaryValue.Nil ->
            printfn "null"
            None
        | PrimaryValue.Expression expr ->
            // todo
            None
    | _ -> Some "Print statement must be followed by a string"

let rec evaluateStatement (expression: Statement): string option =
    match expression with
    | Statement.PrintStatement expr -> evaluatePrintStatement expr
    | Statement.ExpressionStatement expr -> evaluateExpressionStatement expr