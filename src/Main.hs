module Main where

import System.IO (stderr, hPutStrLn)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure, ExitCode(..))
-- import System.FilePath (replaceExtension)
-- import System.Process

import Compiler (compile)


runFile :: FilePath -> IO ()
runFile filePath = do
    latteSourceCode <- readFile filePath
    -- let llvmSourceCodeFilePath = replaceExtension filePath "ll"
    -- let llvmBitcodeFilePath = replaceExtension filePath "bc"
    llvmSourceCode <- compile latteSourceCode
    -- writeFile llvmSourceCodeFilePath llvmSourceCode
    -- (exitcode, out, err) <- readProcessWithExitCode ("llvm-as") ["-o", llvmBitcodeFilePath, llvmSourceCodeFilePath] ""
    -- case exitcode of
    --     ExitSuccess -> do
    --         hPutStrLn stderr "OK"
    --         exitSuccess
    --     ExitFailure i -> do
    --         hPutStrLn stderr $ "An error occurred (exit code: " ++ show i ++ ")"
    --         hPutStrLn stderr out
    --         hPutStrLn stderr err
    --         exitFailure
    hPutStrLn stderr "OK"
    exitSuccess

usage :: IO ()
usage = do
    putStrLn $ unlines
        [ "usage: Call with one of the following argument combinations:"
        , "  --help          Display this help message."
        , "  (files)         Compile content of files."
        ]
    exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    fs -> mapM_ runFile fs
