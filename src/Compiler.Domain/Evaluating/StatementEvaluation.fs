module StatementEvaluation

open Expression
open Statement
open Microsoft.FSharp.Core
open Expression

type EvaluationContext = {
    Variables: Map<string, PrimaryValue>
    Errors: string list
}

let rec printPrimaryValue value context =
    match value with
        | PrimaryValue.String str ->
            printfn "%s" str
            context
        | PrimaryValue.Number int ->
            printfn "%f" int
            context
        | PrimaryValue.Boolean bool ->
            printfn "%b" bool
            context
        | PrimaryValue.Nil ->
            printfn "null"
            context
        | PrimaryValue.Identifier id ->
            let variable = Map.tryFind id context.Variables
            match variable with
            | Some value -> printPrimaryValue value context
            | None -> { context with Errors = sprintf "Variable %s not found" id :: context.Errors}
        | PrimaryValue.Expression expr ->
            printfn "%s" "Coming soon, expression"
            context

let evaluateVariableStatement (statement: VarStatement) (context: EvaluationContext): EvaluationContext =
    let evaluatedExpression = ExpressionEvaluation.evaluateExpression statement.Initializer
    match evaluatedExpression with
    | ExpressionEvaluation.Success value -> { context with Variables = Map.add statement.Name.Lexeme value context.Variables}
    | _ -> { context with Errors = "Expression statement must be followed by an expression" :: context.Errors}

let evaluateExpressionStatement (statement: ExpressionStatement) (context: EvaluationContext): EvaluationContext =
    let evaluatedExpression = ExpressionEvaluation.evaluateExpression statement.Expression
    match evaluatedExpression with
    | ExpressionEvaluation.Success value -> context
    | _ -> { context with Errors = "Expression statement must be followed by an expression" :: context.Errors}

let rec evaluatePrintStatement (statement: PrintStatement) (context: EvaluationContext): EvaluationContext =
    let evaluatedExpression = ExpressionEvaluation.evaluateExpression statement.Expression
    match evaluatedExpression with
    | ExpressionEvaluation.Success value -> printPrimaryValue value context
    | _ -> { context with Errors = "Print statement must be followed by an expression" :: context.Errors}

let evaluateStatement (statement: Statement) (context: EvaluationContext): EvaluationContext =
    match statement with
        | Statement.PrintStatement expr -> evaluatePrintStatement expr context
        | Statement.ExpressionStatement expr -> evaluateExpressionStatement expr context
        | Statement.VarStatement expr -> evaluateVariableStatement expr context

let evaluateStatements (statements: Statement list) (context: EvaluationContext): EvaluationContext =
    List.fold (fun context statement -> evaluateStatement statement context) context statements