module StatementEvaluation

open Evaluation
open Statement
open Microsoft.FSharp.Core

let rec printPrimaryValue value (context: EvaluationContext) =
    match value with
        | PrimaryValue.String str ->
            printfn $"%s{str}"
            context
        | PrimaryValue.Number int ->
            printfn $"%f{int}"
            context
        | PrimaryValue.Boolean bool ->
            printfn $"%b{bool}"
            context
        | PrimaryValue.Nil ->
            printfn "null"
            context
        | PrimaryValue.Identifier id ->
            match context.Find id with
            | Some value -> printPrimaryValue value context
            | None -> 
                printfn $"Variable %s{id} not found"
                context
        | PrimaryValue.Expression _ ->
            printfn "%s" "Coming soon, expression"
            context

let rec evaluateWhileStatement (statement: WhileStatement<Statement>) (context: EvaluationContext) evaluateStatement: EvaluationContext =
    let evaluatedExpression = ExpressionEvaluation.evaluateExpression statement.Condition context
    match evaluatedExpression with
    | ExpressionEvaluation.Success value -> 
        match value with
        | PrimaryValue.Boolean bool ->
            if bool then
                let updatedContext = evaluateStatement statement.Body context
                evaluateWhileStatement statement updatedContext evaluateStatement
            else
                context
        | _ -> { context with Errors = "While statement condition must be a boolean" :: context.Errors}
    | _ -> { context with Errors = "While statement condition must be a boolean" :: context.Errors}

let evaluateIfStatement (statement: IfStatement<Statement>) (context: EvaluationContext) evaluateStatement: EvaluationContext =
    let evaluatedExpression = ExpressionEvaluation.evaluateExpression statement.Condition context
    match evaluatedExpression with
    | ExpressionEvaluation.Success value -> 
        match value with
        | PrimaryValue.Boolean bool ->
            if bool then
                evaluateStatement statement.ThenBranch context
            else
                match statement.ElseBranch with
                | Some elseBranch -> evaluateStatement elseBranch context
                | None -> context
        | _ -> { context with Errors = "If statement condition must be a boolean" :: context.Errors}
    | _ -> { context with Errors = "If statement condition must be a boolean" :: context.Errors}

let evaluateBlockStatement (statement: BlockStatement<Statement>) (context: EvaluationContext) evaluateStatement: EvaluationContext =
    let newContext = { Enclosing = Some context; Variables = Map.empty; Errors = [] }
    let newResult = List.fold (fun currContext statement -> evaluateStatement statement currContext) newContext statement.Statements
    let newVariables = match newResult.Enclosing with
                        | Some enclosing -> enclosing.Variables
                        | None -> Map.empty
    { context with Errors = context.Errors @ newResult.Errors; Variables = newVariables }

let evaluateVariableStatement (statement: VarStatement) (context: EvaluationContext): EvaluationContext =
    let evaluatedExpression = ExpressionEvaluation.evaluateExpression statement.Initializer context
    match evaluatedExpression, statement.RequiresExistingVariable with
    | ExpressionEvaluation.Success value, false -> context.Insert statement.Name.Lexeme value
    | ExpressionEvaluation.Success value, true ->
        match context.Find statement.Name.Lexeme with
        | None -> { context with Errors = "Variable does not exist" :: context.Errors}
        | Some _ -> context.Update statement.Name.Lexeme value
    | _ -> { context with Errors = "Expression statement must be followed by an expression" :: context.Errors}

let evaluateExpressionStatement (statement: ExpressionStatement) (context: EvaluationContext): EvaluationContext =
    let evaluatedExpression = ExpressionEvaluation.evaluateExpression statement.Expression context
    match evaluatedExpression with
    | ExpressionEvaluation.Success _ -> context
    | _ -> { context with Errors = "Expression statement must be followed by an expression" :: context.Errors}

let rec evaluatePrintStatement (statement: PrintStatement) (context: EvaluationContext): EvaluationContext =
    let evaluatedExpression = ExpressionEvaluation.evaluateExpression statement.Expression context
    match evaluatedExpression with
    | ExpressionEvaluation.Success value -> printPrimaryValue value context
    | _ -> { context with Errors = "Print statement must be followed by an expression" :: context.Errors}

let rec evaluateStatement (statement: Statement) (context: EvaluationContext): EvaluationContext =
    match statement with
        | Statement.PrintStatement expr -> evaluatePrintStatement expr context
        | Statement.ExpressionStatement expr -> evaluateExpressionStatement expr context
        | Statement.VarStatement expr -> evaluateVariableStatement expr context
        | Statement.BlockStatement statements -> evaluateBlockStatement statements context evaluateStatement
        | Statement.IfStatement ifStatement -> evaluateIfStatement ifStatement context evaluateStatement
        | Statement.WhileStatement whileStatement -> evaluateWhileStatement whileStatement context evaluateStatement

let evaluateStatements (statements: Statement list) (context: EvaluationContext): EvaluationContext =
    List.fold (fun context statement -> evaluateStatement statement context) context statements