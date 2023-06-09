module Evaluation

open Errors
open Tokens

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

    override this.ToString() =
        match this with
        | BinaryExpr { Left = left; Operator = op; Right = right } -> $"(%s{left.ToString()} %s{op.Lexeme.ToString()} %s{right.ToString()})"
        | UnaryExpr { Operator = op; Right = right } -> $"(%s{op.Lexeme.ToString()} %s{right.ToString()})"
        | LiteralExpr { Value = value } ->
            match value with
            | Token token -> token.Lexeme.ToString()
            | Expression expr -> expr.ToString()

type ExpressionInput = { Tokens: Token list; Errors: SyntaxError list }

type PrimaryValue =
    | Number of float
    | String of string
    | Boolean of bool
    | Nil
    | Expression of Expression
    | Identifier of string

type Variables = Map<string, PrimaryValue>

type EvaluationContext =
    { Enclosing: EvaluationContext option
      Variables: Variables
      Errors: string list }

    member this.Find id =
        let rec findInternal id context =
            match context.Variables.TryGetValue id with
            | true, value -> Some value
            | false, _ ->
                match context.Enclosing with
                | Some enclosing -> findInternal id enclosing
                | None -> None

        match findInternal id this with
        | Some value -> Some value
        | None -> None
        
    member this.Insert id value =
        { this with Variables = this.Variables.Add(id, value) }
        
    member this.Update id value =
        let rec updateInternal id value context =
            match context.Variables.TryGetValue id with
            | true, _ -> { context with Variables = context.Variables.Add(id, value) }
            | false, _ ->
                match context.Enclosing with
                | Some enclosing ->
                    let updatedEnclosing = updateInternal id value enclosing
                    { context with Enclosing = Some updatedEnclosing }
                | None -> context
                
        updateInternal id value this