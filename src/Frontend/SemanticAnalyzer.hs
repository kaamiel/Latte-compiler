module Frontend.SemanticAnalyzer where

import System.Exit (exitFailure)
import System.IO (stderr, hPutStrLn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import AbsLatte
import Frontend.Utils
import Frontend.Optimizer


data Error
    = Error SourceLocation String
  deriving Show

data SemanticAnalysisState = SemanticAnalysisState
    { functions :: Map.Map Ident (Type SourceLocation) -- map function name ~> Fun a (Type a) [Type a]
    , variables :: Map.Map Ident (Type SourceLocation) -- map variable name ~> Int a | Str a | Bool a
    , variablesDeclaredInCurrentBlock :: Set.Set Ident -- set of variable names
    , currentFunctionReturnType :: Type SourceLocation -- Int a | Str a | Bool a | Void a
    }

type AnalyzerStateT = StateT SemanticAnalysisState (Except Error)


noLocation :: SourceLocation
noLocation = Nothing

predefinedFunctions :: Map.Map Ident (Type SourceLocation)
predefinedFunctions =
    Map.fromList [
        (Ident "printInt",    Fun noLocation (Void noLocation) [Int noLocation]), -- void printInt(int)
        (Ident "printString", Fun noLocation (Void noLocation) [Str noLocation]), -- void printString(string)
        (Ident "error",       Fun noLocation (Void noLocation) []),               -- void error()
        (Ident "readInt",     Fun noLocation (Int noLocation)  []),               -- int readInt()
        (Ident "readString",  Fun noLocation (Str noLocation)  [])]               -- string readString()

declaredFunctions :: Program SourceLocation -> Except Error (Map.Map Ident (Type SourceLocation))
declaredFunctions (Program _ topDefs) = do
    funs <- mapM topDefToFun topDefs
    foldM f predefinedFunctions funs
    where
        topDefToFun :: TopDef SourceLocation -> Except Error (Ident, Type SourceLocation)
        topDefToFun (FnDef location returnType name args _) = do
            checkArgs args
            return (name, Fun location returnType $ map argToType args)
        checkArgs :: [Arg SourceLocation] -> Except Error ()
        checkArgs args = foldM_ checkArgAcc Set.empty args
        checkArgAcc :: Set.Set Ident -> Arg SourceLocation -> Except Error (Set.Set Ident)
        checkArgAcc set (Arg location t name@(Ident ident)) = do
            when (voidType t) $ throwError $ Error location "parameter type cannot be `void'"
            when (Set.member name set) $ throwError $ Error location ("redefinition of parameter `" ++ ident ++ "'")
            return $ Set.insert name set
        argToType :: Arg SourceLocation -> Type SourceLocation
        argToType (Arg _ t _) = t
        f :: Map.Map Ident (Type SourceLocation) -> (Ident, Type SourceLocation) -> Except Error (Map.Map Ident (Type SourceLocation))
        f map (name@(Ident ident), fun@(Fun location _ _)) = do
            when (Map.member name map) $ throwError $ Error location ("redefinition of function `" ++ ident ++ "'")
            return $ Map.insert name fun map


checkDeclarationOfMain :: Map.Map Ident (Type SourceLocation) -> Except Error ()
checkDeclarationOfMain functions = do
    mainType <- maybe (throwError $ Error noLocation "`main' was not defined") return (Map.lookup (Ident "main") functions)
    case mainType of
        Fun _ (Int _) [] -> return ()
        Fun location _ _ -> throwError $ Error location "`main' must return `int' and take no arguments"


checkExpr :: Expr SourceLocation -> AnalyzerStateT (Type SourceLocation)

checkExpr (EVar location x@(Ident ident)) = do
    maybeVarType <- gets $ Map.lookup x . variables
    varType <- maybe (throwError $ Error location ("undeclared variable `" ++ ident ++ "'")) return maybeVarType
    return varType

checkExpr (ELitInt location _) = return $ Int location

checkExpr (ELitTrue location) = return $ Bool location

checkExpr (ELitFalse location) = return $ Bool location

checkExpr (EApp location f@(Ident ident) args) = do
    maybeFunType <- gets $ Map.lookup f . functions
    funType <- maybe (throwError $ Error location ("undeclared function `" ++ ident ++ "'")) return maybeFunType
    actualArgs <- mapM checkExpr args
    case funType of
        Fun _ returnType formalArgs -> do
            let formalArgsLength = length formalArgs
            let actualArgsLength = length actualArgs
            when (actualArgsLength < formalArgsLength) $ throwError $ Error location ("too few arguments passed to function `" ++ ident ++ "'")
            when (formalArgsLength < actualArgsLength) $ throwError $ Error location ("too many arguments passed to function `" ++ ident ++ "'")
            zipWithM_ (\formalArg actualArg -> unless (compatibleNonVoidTypes formalArg actualArg) $ throwError $ Error location ("invalid type of argument passed to function `" ++ ident ++ "'")) formalArgs actualArgs
            return returnType

checkExpr (EString location _) = return $ Str location

checkExpr (Neg location e) = do
    exprType <- checkExpr e
    case exprType of
        Int _ -> return $ Int location
        _     -> throwError $ Error (sourceLocationOfExpr e) "invalid type of expression, expected `int'"

checkExpr (Not location e) = do
    exprType <- checkExpr e
    case exprType of
        Bool _ -> return $ Bool location
        _      -> throwError $ Error (sourceLocationOfExpr e) "invalid type of expression, expected `boolean'"

checkExpr (EMul location e1 mulOp e2) = do
    expr1Type <- checkExpr e1
    expr2Type <- checkExpr e2
    case (expr1Type, expr2Type) of
        (Int _, Int _) -> return $ Int location
        (Int _, _)     -> throwError $ Error (sourceLocationOfExpr e2) "invalid type of expression, expected `int'"
        _              -> throwError $ Error (sourceLocationOfExpr e1) "invalid type of expression, expected `int'"

checkExpr (EAdd location e1 addOp e2) = do
    expr1Type <- checkExpr e1
    expr2Type <- checkExpr e2
    case (addOp, expr1Type, expr2Type) of
        (_,      Int _, Int _) -> return $ Int location
        (Plus _, Str _, Str _) -> return $ Str location
        (_,      Int _, _    ) -> throwError $ Error (sourceLocationOfExpr e2) "invalid type of expression, expected `int'"
        (Plus _, Str _, _    ) -> throwError $ Error (sourceLocationOfExpr e2) "invalid type of expression, expected `string'"
        (_,      _,     Int _) -> throwError $ Error (sourceLocationOfExpr e1) "invalid type of expression, expected `int'"
        (Plus _, _,     Str _) -> throwError $ Error (sourceLocationOfExpr e1) "invalid type of expression, expected `string'"
        (Plus _, _,     _    ) -> throwError $ Error (sourceLocationOfExpr e1) "invalid type of expression, expected `int' or `string'"
        _                      -> throwError $ Error (sourceLocationOfExpr e1) "invalid type of expression, expected `int'"

checkExpr (ERel location e1 relOp e2) = do
    expr1Type <- checkExpr e1
    expr2Type <- checkExpr e2
    when (voidType expr1Type) $ throwError $ Error (sourceLocationOfExpr e1) "invalid type of expression"
    case (relOp, expr1Type, expr2Type) of
        (EQU _, _,      _     ) -> if compatibleNonVoidTypes expr1Type expr2Type then return $ Bool location else throwError $ Error (sourceLocationOfExpr e2) "invalid type of expression"
        (NE _,  _,      _     ) -> if compatibleNonVoidTypes expr1Type expr2Type then return $ Bool location else throwError $ Error (sourceLocationOfExpr e2) "invalid type of expression"
        (_,     Int _,  Int _ ) -> return $ Bool location
        (_,     Bool _, Bool _) -> return $ Bool location
        (_,     Int _,  _     ) -> throwError $ Error (sourceLocationOfExpr e2) "invalid type of expression, expected `int'"
        (_,     Bool _, _     ) -> throwError $ Error (sourceLocationOfExpr e2) "invalid type of expression, expected `boolean'"
        (_,     _,      Int _ ) -> throwError $ Error (sourceLocationOfExpr e1) "invalid type of expression, expected `int'"
        (_,     _,      Bool _) -> throwError $ Error (sourceLocationOfExpr e1) "invalid type of expression, expected `boolean'"
        _                       -> throwError $ Error (sourceLocationOfExpr e1) "invalid type of expression, expected `int' or `boolean'"

checkExpr (EAnd location e1 e2) = do
    expr1Type <- checkExpr e1
    expr2Type <- checkExpr e2
    case (expr1Type, expr2Type) of
        (Bool _, Bool _) -> return $ Bool location
        (Bool _, _)      -> throwError $ Error (sourceLocationOfExpr e2) "invalid type of expression, expected `boolean'"
        _                -> throwError $ Error (sourceLocationOfExpr e1) "invalid type of expression, expected `boolean'"

checkExpr (EOr location e1 e2) = checkExpr $ EAnd location e1 e2


checkStmt :: Stmt SourceLocation -> AnalyzerStateT ()

checkStmt (Empty _) = return ()

checkStmt (BStmt _ (Block _ stmts)) = do
    (variables', variablesDeclaredInCurrentBlock') <- gets $ \state -> (variables state, variablesDeclaredInCurrentBlock state)
    modify $ \state -> state { variablesDeclaredInCurrentBlock = Set.empty }
    mapM_ checkStmt stmts
    modify $ \state -> state { variables = variables', variablesDeclaredInCurrentBlock = variablesDeclaredInCurrentBlock' }

checkStmt (Decl location t items) = do
    when (voidType t) $ throwError $ Error location "variable type cannot be `void'"
    mapM_ checkItem items
    where
        checkItem :: Item SourceLocation -> AnalyzerStateT ()
        checkItem (NoInit location x@(Ident ident)) = do
            declared <- gets $ Set.member x . variablesDeclaredInCurrentBlock
            when declared $ throwError $ Error location ("redeclaration of variable `" ++ ident ++ "'")
            modify $ \state -> state { variables = Map.insert x t (variables state), variablesDeclaredInCurrentBlock = Set.insert x (variablesDeclaredInCurrentBlock state) }
        checkItem (Init location x e) = do
            exprType <- checkExpr e
            unless (compatibleNonVoidTypes t exprType) $ throwError $ Error (sourceLocationOfExpr e) "invalid type of expression"
            checkItem (NoInit location x)

checkStmt (Ass location x e) = do
    varType <- checkExpr (EVar location x)
    exprType <- checkExpr e
    unless (compatibleNonVoidTypes varType exprType) $ throwError $ Error (sourceLocationOfExpr e) "invalid type of expression"

checkStmt (Incr location x@(Ident ident)) = do
    varType <- checkExpr (EVar location x)
    case varType of
        (Int _) -> return ()
        _       -> throwError $ Error location $ "invalid type of variable `" ++ ident ++ "', expected `int'"

checkStmt (Decr location x) = checkStmt $ Incr location x

checkStmt (Ret location e) = do
    exprType <- checkExpr e
    returnType <- gets currentFunctionReturnType
    if voidType returnType
    then
        throwError $ Error location "`return' with a value, in function returning void"
    else
        unless (compatibleNonVoidTypes returnType exprType) $ throwError $ Error (sourceLocationOfExpr e) "invalid type of expression"

checkStmt (VRet location) = do
    returnType <- gets currentFunctionReturnType
    unless (voidType returnType) $ throwError $ Error location "`return' with no value, in function returning non-void"

checkStmt (Cond location conditionExpr thenStmt) = checkStmt $ CondElse location conditionExpr thenStmt (Empty location)

checkStmt (CondElse _ conditionExpr thenStmt elseStmt) = do
    exprType <- checkExpr conditionExpr
    case exprType of
        Bool _ -> do
            checkStmt thenStmt
            checkStmt elseStmt
        _ -> throwError $ Error (sourceLocationOfExpr conditionExpr) "invalid type of expression, expected `boolean'"

checkStmt (While location conditionExpr bodyStmt) = checkStmt $ Cond location conditionExpr bodyStmt

checkStmt (SExp _ e) = do
    checkExpr e
    return ()


checkProgram :: Program SourceLocation -> Map.Map Ident (Type SourceLocation) -> Except Error ()
checkProgram (Program _ topDefs) functions =
    mapM_ checkTopDef topDefs
    where
        checkTopDef :: TopDef SourceLocation -> Except Error ()
        checkTopDef (FnDef _ returnType _ args (Block _ stmts)) =
            let
                argNamesAndTypes = map argNameAndType args
                variables = Map.fromList argNamesAndTypes
                variablesDeclaredInCurrentBlock = Map.keysSet variables
                initialState = SemanticAnalysisState functions variables variablesDeclaredInCurrentBlock returnType
            in
            flip evalStateT initialState $ mapM_ checkStmt stmts
        argNameAndType :: Arg SourceLocation -> (Ident, Type SourceLocation)
        argNameAndType (Arg _ t name) = (name, t)


allPathsReturnValue :: Stmt SourceLocation -> Bool

allPathsReturnValue (BStmt _ (Block _ stmts)) = any allPathsReturnValue stmts

allPathsReturnValue (Ret _ _) = True

allPathsReturnValue (Cond _ conditionExpr thenStmt) =
    isBoolConstExpr conditionExpr
    && evalBoolConstExpr conditionExpr
    && allPathsReturnValue thenStmt

allPathsReturnValue (CondElse _ conditionExpr thenStmt elseStmt) =
    allThenStmtPathsReturnValue && allElseStmtPathsReturnValue
    || isBoolConstExpr conditionExpr && (evalBoolConstExpr conditionExpr && allThenStmtPathsReturnValue || allElseStmtPathsReturnValue)
    where
        allThenStmtPathsReturnValue = allPathsReturnValue thenStmt
        allElseStmtPathsReturnValue = allPathsReturnValue elseStmt

allPathsReturnValue (While _ conditionExpr bodyStmt) =
    isBoolConstExpr conditionExpr
    && evalBoolConstExpr conditionExpr

allPathsReturnValue _ = False


checkReturns :: Program SourceLocation -> Except Error ()
checkReturns (Program location topDefs) =
    mapM_ checkTopDef topDefs
    where
        checkTopDef :: TopDef SourceLocation -> Except Error ()
        checkTopDef (FnDef location returnType _ _ bodyBlock) =
            if voidType returnType
            then
                return ()
            else
                unless (allPathsReturnValue $ BStmt location bodyBlock) $ throwError $ Error (sourceLocationOfBlocksLastStatement bodyBlock) "every execution path must return a value in function returning non-void"


analyzeInternal :: Program SourceLocation -> Except Error (Program SourceLocation, Map.Map Ident (Type SourceLocation))
analyzeInternal program = do
    functions <- declaredFunctions program
    checkDeclarationOfMain functions
    checkProgram program functions
    let optimizedProgram = optimizeProgram program
    checkReturns optimizedProgram
    return (optimizedProgram, functions)

analyze :: Program SourceLocation -> IO (Program SourceLocation, Map.Map Ident (Type SourceLocation))
analyze program =
    case runExcept . analyzeInternal $ program of
        Right ret -> return ret
        Left (Error location message) -> do
            let l = maybe "unknown location" (\(line, column) -> "line " ++ show line ++ ", column " ++ show column) location
            hPutStrLn stderr "ERROR"
            hPutStrLn stderr $ "at " ++ l
            hPutStrLn stderr message
            exitFailure
