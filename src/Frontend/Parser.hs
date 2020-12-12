module Frontend.Parser (parse) where

import System.Exit (exitFailure)
import System.IO (stderr, hPutStrLn)
import ParLatte
import AbsLatte
import ErrM
import Frontend.Utils (SourceLocation(..))


parse :: String -> IO (Program SourceLocation)
parse source =
    let
        ts = myLexer source
    in
    case pProgram ts of
        Bad s -> do
            hPutStrLn stderr "ERROR"
            hPutStrLn stderr "Parse failed"
            hPutStrLn stderr s
            exitFailure
        Ok tree -> return tree
