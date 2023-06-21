module Tokenizer

open Tokens

let tokenize (internalFn: TokenizerInput -> Token list -> Token list) input = internalFn { Input = input; Column = 0; Line = 1 } []
