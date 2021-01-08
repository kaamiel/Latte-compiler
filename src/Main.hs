module Main where

import System.IO (stderr, hPutStrLn)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure, ExitCode(..))
import System.FilePath (replaceExtension)
import System.Process (readProcessWithExitCode)

import Compiler (compile)


runCommand :: FilePath -> [String] -> IO ()
runCommand command args = do
    (exitcode, out, err) <- readProcessWithExitCode command args ""
    case exitcode of
        ExitSuccess -> return ()
        ExitFailure i -> do
            hPutStrLn stderr $ "An error occurred (exit code: " ++ show i ++ ")"
            hPutStrLn stderr out
            hPutStrLn stderr err
            exitFailure

runFile :: FilePath -> IO ()
runFile filePath = do
    latteSourceCode <- readFile filePath
    let llvmSourceCodeFilePath = replaceExtension filePath "ll"
    let llvmBitcodeFilePath = replaceExtension filePath "bc"
    llvmSourceCode <- compile latteSourceCode
    writeFile llvmSourceCodeFilePath llvmSourceCode
    runCommand "llvm-as" ["-o", llvmBitcodeFilePath, llvmSourceCodeFilePath]
    runCommand "llvm-link" ["-o", llvmBitcodeFilePath, llvmBitcodeFilePath, "./lib/runtime.bc"]
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
