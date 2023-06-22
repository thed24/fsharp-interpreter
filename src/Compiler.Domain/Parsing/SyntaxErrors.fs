module SyntaxErrors

open Tokens

type UnexpectedEndOfInputError = { Message: string }
type MissingRightOperandError = { Line: int; Column: int; Operator: Token }

type SyntaxError =
    | UnexpectedEndOfInput of UnexpectedEndOfInputError
    | MissingRightOperand of MissingRightOperandError
