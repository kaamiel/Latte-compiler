module SemanticAnalyzerTests where

import Frontend.Utils
import Frontend.SemanticAnalyzer
import AbsLatte
import Data.Either
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Except
import Control.Monad.State


{- | declaredFunctions tests

>>> runExcept . declaredFunctions $ Program Nothing []
Right (fromList [(Ident "error",Fun Nothing (Void Nothing) []),(Ident "printInt",Fun Nothing (Void Nothing) [Int Nothing]),(Ident "printString",Fun Nothing (Void Nothing) [Str Nothing]),(Ident "readInt",Fun Nothing (Int Nothing) []),(Ident "readString",Fun Nothing (Str Nothing) [])])

>>> :{
let
    program :: Program SourceLocation
    program = Program Nothing
        [FnDef Nothing (Int Nothing)
            (Ident "main") [] $ Block Nothing $ []]
in
runExcept . declaredFunctions $ program
:}
Right (fromList [(Ident "error",Fun Nothing (Void Nothing) []),(Ident "main",Fun Nothing (Int Nothing) []),(Ident "printInt",Fun Nothing (Void Nothing) [Int Nothing]),(Ident "printString",Fun Nothing (Void Nothing) [Str Nothing]),(Ident "readInt",Fun Nothing (Int Nothing) []),(Ident "readString",Fun Nothing (Str Nothing) [])])

>>> :{
let
    program :: Program SourceLocation
    program = Program Nothing
        [FnDef Nothing (Int Nothing)
            (Ident "f") [Arg Nothing (Str Nothing) (Ident "s"), Arg Nothing (Bool Nothing) (Ident "b")] $ Block Nothing $ [],
        FnDef Nothing (Int Nothing)
            (Ident "main") [] $ Block Nothing $ []]
in
runExcept . declaredFunctions $ program
:}
Right (fromList [(Ident "error",Fun Nothing (Void Nothing) []),(Ident "f",Fun Nothing (Int Nothing) [Str Nothing,Bool Nothing]),(Ident "main",Fun Nothing (Int Nothing) []),(Ident "printInt",Fun Nothing (Void Nothing) [Int Nothing]),(Ident "printString",Fun Nothing (Void Nothing) [Str Nothing]),(Ident "readInt",Fun Nothing (Int Nothing) []),(Ident "readString",Fun Nothing (Str Nothing) [])])

>>> :{
let
    program :: Program SourceLocation
    program = Program Nothing
        [FnDef Nothing (Int Nothing)
            (Ident "main") [Arg Nothing (Str Nothing) (Ident "s"), Arg Nothing (Bool Nothing) (Ident "b")] $ Block Nothing $ [],
        FnDef (Just (1,2)) (Int Nothing)
            (Ident "main") [] $ Block Nothing $ []]
in
runExcept . declaredFunctions $ program
:}
Left (Error (Just (1,2)) "redefinition of function `main'")

>>> :{
let
    program :: Program SourceLocation
    program = Program Nothing
        [FnDef Nothing (Int Nothing)
            (Ident "main") [Arg Nothing (Int Nothing) (Ident "k"), Arg (Just (3, 4)) (Int Nothing) (Ident "k")] $ Block Nothing $ []]
in
runExcept . declaredFunctions $ program
:}
Left (Error (Just (3,4)) "redefinition of parameter `k'")

>>> :{
let
    program :: Program SourceLocation
    program = Program Nothing
        [FnDef Nothing (Int Nothing)
            (Ident "main") [Arg Nothing (Int Nothing) (Ident "k"), Arg Nothing (Str Nothing) (Ident "s"), Arg (Just (5, 6)) (Bool Nothing) (Ident "k")] $ Block Nothing $ []]
in
runExcept . declaredFunctions $ program
:}
Left (Error (Just (5,6)) "redefinition of parameter `k'")

-}



{- | checkDeclarationOfMain tests

>>> runExcept . checkDeclarationOfMain $ Map.singleton (Ident "main") (Fun Nothing (Int Nothing) [])
Right ()

>>> runExcept . checkDeclarationOfMain $ Map.empty
Left (Error Nothing "`main' was not defined")

>>> runExcept . checkDeclarationOfMain $ Map.singleton (Ident "mainn") (Fun Nothing (Int Nothing) [])
Left (Error Nothing "`main' was not defined")

>>> runExcept . checkDeclarationOfMain $ Map.singleton (Ident "main") (Fun (Just (7, 8)) (Void Nothing) [])
Left (Error (Just (7,8)) "`main' must return `int' and take no arguments")

>>> runExcept . checkDeclarationOfMain $ Map.singleton (Ident "main") (Fun (Just (9, 10)) (Int Nothing) [Bool Nothing])
Left (Error (Just (9,10)) "`main' must return `int' and take no arguments")

-}



{- | checkExpr tests

>>> :{
let
    variables :: Map.Map Ident (Type SourceLocation)
    variables = Map.singleton (Ident "x") (Int Nothing)
in
runExcept . flip evalStateT (SemanticAnalysisState Map.empty variables Set.empty (Void Nothing)) . checkExpr $ EVar Nothing (Ident "x")
:}
Right (Int Nothing)

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ EVar (Just (11,12)) (Ident "y")
Left (Error (Just (11,12)) "undeclared variable `y'")

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ ELitInt Nothing 42
Right (Int Nothing)

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ ELitTrue Nothing
Right (Bool Nothing)

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ ELitFalse Nothing
Right (Bool Nothing)

>>> :{
let
    functions :: Map.Map Ident (Type SourceLocation)
    functions = Map.singleton (Ident "f") (Fun Nothing (Bool Nothing) [])
in
runExcept . flip evalStateT (SemanticAnalysisState functions Map.empty Set.empty (Void Nothing)) . checkExpr $ EApp Nothing (Ident "f") []
:}
Right (Bool Nothing)

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ EApp (Just (13, 14)) (Ident "g") []
Left (Error (Just (13,14)) "undeclared function `g'")

>>> :{
let
    functions :: Map.Map Ident (Type SourceLocation)
    functions = Map.singleton (Ident "f") (Fun Nothing (Bool Nothing) [Int Nothing])
in
runExcept . flip evalStateT (SemanticAnalysisState functions Map.empty Set.empty (Void Nothing)) . checkExpr $ EApp (Just (14, 15)) (Ident "f") []
:}
Left (Error (Just (14,15)) "too few arguments passed to function `f'")

>>> :{
let
    functions :: Map.Map Ident (Type SourceLocation)
    functions = Map.singleton (Ident "f") (Fun Nothing (Bool Nothing) [])
in
runExcept . flip evalStateT (SemanticAnalysisState functions Map.empty Set.empty (Void Nothing)) . checkExpr $ EApp (Just (15, 16)) (Ident "f") [ELitInt Nothing 4]
:}
Left (Error (Just (15,16)) "too many arguments passed to function `f'")

>>> :{
let
    functions :: Map.Map Ident (Type SourceLocation)
    functions = Map.singleton (Ident "f") (Fun Nothing (Bool Nothing) [Str Nothing])
in
runExcept . flip evalStateT (SemanticAnalysisState functions Map.empty Set.empty (Void Nothing)) . checkExpr $ EApp (Just (16, 17)) (Ident "f") [ELitInt Nothing 5]
:}
Left (Error (Just (16,17)) "invalid type of argument passed to function `f'")

>>> :{
let
    functions :: Map.Map Ident (Type SourceLocation)
    functions = Map.singleton (Ident "f") (Fun Nothing (Bool Nothing) [Int Nothing])
in
runExcept . flip evalStateT (SemanticAnalysisState functions Map.empty Set.empty (Void Nothing)) . checkExpr $ EApp Nothing (Ident "f") [EVar (Just (17, 18)) (Ident "x")]
:}
Left (Error (Just (17,18)) "undeclared variable `x'")

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ EString Nothing "ala"
Right (Str Nothing)

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ Neg Nothing (ELitInt Nothing 17)
Right (Int Nothing)

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ Neg Nothing (ELitTrue (Just (18, 19)))
Left (Error (Just (18,19)) "invalid type of expression, expected `int'")

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ Not Nothing (ELitTrue Nothing)
Right (Bool Nothing)

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ Not Nothing (ELitInt (Just (19, 20)) 17)
Left (Error (Just (19,20)) "invalid type of expression, expected `boolean'")

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ EMul Nothing (ELitInt Nothing 1) (Times Nothing) (ELitInt Nothing 7)
Right (Int Nothing)

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ EMul Nothing (ELitTrue (Just (20, 21))) (Times Nothing) (ELitInt Nothing 7)
Left (Error (Just (20,21)) "invalid type of expression, expected `int'")

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ EMul Nothing (ELitInt Nothing 5) (Times Nothing) (ELitTrue (Just (21, 22)))
Left (Error (Just (21,22)) "invalid type of expression, expected `int'")

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ ERel Nothing (ELitInt Nothing 1) (LTH Nothing) (ELitInt Nothing 7)
Right (Bool Nothing)

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ ERel Nothing (ELitTrue Nothing) (LTH Nothing) (ELitFalse Nothing)
Right (Bool Nothing)

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ ERel Nothing (ELitInt Nothing 7) (LTH Nothing) (EString (Just (22, 23)) "ma")
Left (Error (Just (22,23)) "invalid type of expression, expected `int'")

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ ERel Nothing (ELitTrue Nothing) (LTH Nothing) (EString (Just (23, 24)) "kota")
Left (Error (Just (23,24)) "invalid type of expression, expected `boolean'")

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ ERel Nothing (EString (Just (24, 25)) "a nie psa") (LE Nothing) (ELitInt Nothing 7)
Left (Error (Just (24,25)) "invalid type of expression, expected `int'")

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ EAnd Nothing (ELitTrue Nothing) (ELitFalse Nothing)
Right (Bool Nothing)

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ EAnd Nothing (ELitTrue Nothing) (ELitInt (Just (25, 26)) 7)
Left (Error (Just (25,26)) "invalid type of expression, expected `boolean'")

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ EAnd Nothing (ELitInt (Just (26, 27)) 5) (ELitTrue Nothing)
Left (Error (Just (26,27)) "invalid type of expression, expected `boolean'")

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ EOr Nothing (ELitTrue Nothing) (ELitFalse Nothing)
Right (Bool Nothing)

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ EOr Nothing (ELitTrue Nothing) (ELitInt (Just (27, 28)) 7)
Left (Error (Just (27,28)) "invalid type of expression, expected `boolean'")

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ EOr Nothing (ELitInt (Just (28, 29)) 5) (ELitTrue Nothing)
Left (Error (Just (28,29)) "invalid type of expression, expected `boolean'")

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ EAdd Nothing (ELitInt Nothing 1) (Plus Nothing) (ELitInt Nothing 7)
Right (Int Nothing)

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ EAdd Nothing (ELitInt Nothing 1) (Minus Nothing) (ELitInt Nothing 7)
Right (Int Nothing)

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ EAdd Nothing (EString Nothing "cc") (Plus Nothing) (EString Nothing "dd")
Right (Str Nothing)

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ EAdd Nothing (EString (Just (29, 30)) "cc") (Minus Nothing) (EString Nothing "dd")
Left (Error (Just (29,30)) "invalid type of expression, expected `int'")

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ EAdd Nothing (ELitInt Nothing 1) (Plus Nothing) (ELitTrue (Just (30, 31)))
Left (Error (Just (30,31)) "invalid type of expression, expected `int'")

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ EAdd Nothing (ELitTrue (Just (31, 32))) (Plus Nothing) (ELitInt Nothing 3)
Left (Error (Just (31,32)) "invalid type of expression, expected `int'")

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ EAdd Nothing (ELitTrue (Just (32, 33))) (Minus Nothing) (ELitInt Nothing 3)
Left (Error (Just (32,33)) "invalid type of expression, expected `int'")

>>> runExcept . flip evalStateT (SemanticAnalysisState Map.empty Map.empty Set.empty (Void Nothing)) . checkExpr $ EAdd Nothing (EString Nothing "ee") (Plus Nothing) (ELitTrue (Just (33, 34)))
Left (Error (Just (33,34)) "invalid type of expression, expected `string'")

-}


