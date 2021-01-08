module Compiler (compile) where

import Frontend.Parser (parse)
import Frontend.SemanticAnalyzer (analyze)
import Backend.CodeGenerator (generateCode)


compile :: String -> IO String
compile source = do
    inputProgram <- parse source
    (correctProgram, functions) <- analyze inputProgram
    return $ generateCode correctProgram functions
