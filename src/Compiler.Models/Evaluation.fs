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