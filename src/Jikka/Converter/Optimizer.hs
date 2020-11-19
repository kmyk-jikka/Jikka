module Jikka.Converter.Optimizer (run) where

import Jikka.Language.Python.Typed.Type (Program)

run :: Program -> Either String Program
run = return
