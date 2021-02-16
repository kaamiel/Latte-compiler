module Backend.CodeGenerator (generateCode) where

import qualified Data.Map as Map
import Data.List (foldl', sort)
import Data.Maybe
import Control.Monad.State
import AbsLatte
import Backend.LLVMAsm
import Backend.Optimizer


data VariableValues = VariableValues
    { stack     :: [Value]
    , stackSize :: Int
    }

data CodeGenerationState = CodeGenerationState
    { nextRegisterNumber :: Int
    , nextLabelNumber    :: Int
    , currentLabel       :: Label
    , variables          :: Map.Map Ident VariableValues -- map variable name ~> variable values
    , functions          :: Map.Map Ident Ty             -- map function name ~> return Ty
    , stringLiterals     :: Map.Map String Name          -- map string literal ~> array constant name
    , code               :: [Instruction]                -- reversed list of generated instructions
    , instructionResults :: Map.Map Instruction Name     -- map instruction ~> name of register with result
    }

type GeneratorStateT = State CodeGenerationState


declarations :: String
declarations =
    "declare void @printInt(i32)\n\
    \declare void @printString(i8*)\n\
    \declare void @error()\n\
    \declare i32  @readInt()\n\
    \declare i8*  @readString()\n\
    \declare i8*  @_concatStrings(i8*, i8*)\n\n"

freshRegister :: GeneratorStateT Name
freshRegister = do
    r <- gets nextRegisterNumber
    modify $ \state -> state { nextRegisterNumber = r + 1 }
    return $ "%_r" ++ show r

freshLabel :: GeneratorStateT Label
freshLabel = do
    l <- gets nextLabelNumber
    modify $ \state -> state { nextLabelNumber = l + 1 }
    return $ "%_L" ++ show l

emit :: Instruction -> GeneratorStateT ()
emit i@(Label label) = modify $ \state -> state { code = i : code state, currentLabel = label }
emit i               = modify $ \state -> state { code = i : code state }

emitWithGCSE :: Instruction -> GeneratorStateT Name
emitWithGCSE i = do
    results <- gets instructionResults
    maybe rememberNewResult return (Map.lookup i results)
    where
        rememberNewResult :: GeneratorStateT Name
        rememberNewResult = do
            r <- freshRegister
            modify $ \state -> state { instructionResults = foldr (flip Map.insert r) (instructionResults state) (equalInstructions i) }
            emit $ insertResult r i
            return r
        insertResult :: Name -> Instruction -> Instruction
        insertResult r (Add _ op1 op2)            = Add r op1 op2
        insertResult r (Sub _ op1 op2)            = Sub r op1 op2
        insertResult r (Mul _ op1 op2)            = Mul r op1 op2
        insertResult r (Sdiv _ op1 op2)           = Sdiv r op1 op2
        insertResult r (Srem _ op1 op2)           = Srem r op1 op2
        insertResult r (Xor _ op1 op2)            = Xor r op1 op2
        insertResult r (Icmp _ contidion op1 op2) = Icmp r contidion op1 op2
        insertResult r (Call _ ty name args)      = Call r ty name args
        insertResult r (Phi _ values)             = Phi r values
        insertResult r (Bitcast _ n pointer)      = Bitcast r n pointer
        insertResult _ i                          = i

top :: VariableValues -> Value
top = head . stack

changeTop :: Value -> VariableValues -> VariableValues
changeTop newTop values = values { stack = newTop : tail (stack values) }

getVariableValue :: Ident -> GeneratorStateT Value
getVariableValue x = gets $ top . fromJust . Map.lookup x . variables

changeVariableValue :: Ident -> Value -> GeneratorStateT ()
changeVariableValue x newValue = modify $ \state -> state { variables = Map.adjust (changeTop newValue) x $ variables state }

setVariables :: Map.Map Ident VariableValues -> GeneratorStateT ()
setVariables newVariables = modify $ \state -> state { variables = newVariables }


definedStringLiterals :: Program a -> Map.Map String Name
definedStringLiterals (Program _ topDefs) =
    foldr topDefsStringsAcc Map.empty topDefs
    where
        topDefsStringsAcc :: TopDef a -> Map.Map String Name -> Map.Map String Name
        topDefsStringsAcc (FnDef _ _ _ _ (Block _ stmts)) strs = foldr stmtsStringsAcc strs stmts
        stmtsStringsAcc :: Stmt a -> Map.Map String Name -> Map.Map String Name
        stmtsStringsAcc (BStmt _ (Block _ stmts)) strs                    = foldr stmtsStringsAcc strs stmts
        stmtsStringsAcc (Decl _ t items) strs                             = foldr (itemsStringsAcc t) strs items
        stmtsStringsAcc (Ass _ _ e) strs                                  = exprsStringsAcc e strs
        stmtsStringsAcc (Ret _ e) strs                                    = exprsStringsAcc e strs
        stmtsStringsAcc (Cond _ conditionExpr thenStmt) strs              = exprsStringsAcc conditionExpr $ stmtsStringsAcc thenStmt strs
        stmtsStringsAcc (CondElse _ conditionExpr thenStmt elseStmt) strs = exprsStringsAcc conditionExpr $ stmtsStringsAcc thenStmt $ stmtsStringsAcc elseStmt strs
        stmtsStringsAcc (While _ conditionExpr bodyStmt) strs             = exprsStringsAcc conditionExpr $ stmtsStringsAcc bodyStmt strs
        stmtsStringsAcc (SExp _ e) strs                                   = exprsStringsAcc e strs
        stmtsStringsAcc _ strs                                            = strs
        exprsStringsAcc :: Expr a -> Map.Map String Name -> Map.Map String Name
        exprsStringsAcc (EApp _ _ args) strs    = foldr exprsStringsAcc strs args
        exprsStringsAcc (EString _ string) strs = defineStringLiteral (if length string < 2 then string else init . tail $ string) strs
        exprsStringsAcc (Neg _ e) strs          = exprsStringsAcc e strs
        exprsStringsAcc (Not _ e) strs          = exprsStringsAcc e strs
        exprsStringsAcc (EMul _ e1 _ e2) strs   = exprsStringsAcc e1 $ exprsStringsAcc e2 strs
        exprsStringsAcc (EAdd _ e1 _ e2) strs   = exprsStringsAcc e1 $ exprsStringsAcc e2 strs
        exprsStringsAcc (ERel _ e1 _ e2) strs   = exprsStringsAcc e1 $ exprsStringsAcc e2 strs
        exprsStringsAcc (EAnd _ e1 e2) strs     = exprsStringsAcc e1 $ exprsStringsAcc e2 strs
        exprsStringsAcc (EOr _ e1 e2) strs      = exprsStringsAcc e1 $ exprsStringsAcc e2 strs
        exprsStringsAcc _ strs                  = strs
        itemsStringsAcc :: Type a -> Item a -> Map.Map String Name -> Map.Map String Name
        itemsStringsAcc (Str _) (NoInit _ _) strs = defineStringLiteral "" strs
        itemsStringsAcc _       (Init _ _ e) strs = exprsStringsAcc e strs
        itemsStringsAcc _ _ strs                  = strs
        defineStringLiteral :: String -> Map.Map String Name -> Map.Map String Name
        defineStringLiteral string strings =
            if Map.member string strings
            then
                strings
            else
                Map.insert string ("@_s" ++ show (Map.size strings)) strings


generateExpr :: Expr a -> GeneratorStateT Value

generateExpr (EVar _ x) = getVariableValue x

generateExpr (ELitInt _ n) = return $ IntegerValue n

generateExpr (ELitTrue _) = return $ BoolValue True

generateExpr (ELitFalse _) = return $ BoolValue False

generateExpr (EApp _ f@(Ident ident) args) = do
    returnTy <- gets $ fromJust . Map.lookup f . functions
    argValues <- mapM generateExpr args
    if returnTy == VoidTy
    then do
        emit $ CallVoid ("@" ++ ident) argValues
        return NoValue
    else do
        r <- freshRegister
        emit $ Call r returnTy ("@" ++ ident) argValues
        return $ RegisterValue returnTy r

generateExpr (EString _ string) = do
    let string' = if length string < 2 then string else init . tail $ string
    name <- gets $ fromJust . Map.lookup string' . stringLiterals
    r <- emitWithGCSE $ Bitcast "_" (length string' + 1) name
    return $ RegisterValue (Ptr I8) r

generateExpr (Neg location e) = generateIntegerBinaryOperation Sub (-) (ELitInt location 0) e

generateExpr (Not location e) = generateBoolBinaryOperation Xor (/=) (ELitTrue location) e

generateExpr (EMul _ e1 (Times _) e2) = generateIntegerBinaryOperation Mul (*) e1 e2

generateExpr (EMul _ e1 (Div _) e2) = generateIntegerBinaryOperation Sdiv div e1 e2

generateExpr (EMul _ e1 (Mod _) e2) = generateIntegerBinaryOperation Srem mod e1 e2

generateExpr (EAdd _ e1 (Plus _) e2) = do
    value1 <- generateExpr e1
    value2 <- generateExpr e2
    case (value1, value2) of
        (IntegerValue n1, IntegerValue n2) -> return $ IntegerValue (n1 + n2)
        (RegisterValue (Ptr I8) _, RegisterValue (Ptr I8) _) -> do
            r <- emitWithGCSE $ Call "_" (Ptr I8) "@_concatStrings" [value1, value2]
            return $ RegisterValue (Ptr I8) r
        _ -> do
            r <- emitWithGCSE $ Add "_" value1 value2
            return $ RegisterValue I32 r

generateExpr (EAdd _ e1 (Minus _) e2) = generateIntegerBinaryOperation Sub (-) e1 e2

generateExpr (ERel _ e1 relOp e2) = do
    value1 <- generateExpr e1
    value2 <- generateExpr e2
    case (value1, value2) of
        (RegisterValue _ r1, RegisterValue _ r2) ->
            if r1 == r2
            then
                return $ BoolValue (op () ())
            else
                icmp value1 value2
        (IntegerValue n1, IntegerValue n2) -> return $ BoolValue (op n1 n2)
        (BoolValue b1, BoolValue b2) -> return $ BoolValue (op b1 b2)
        _ -> icmp value1 value2
    where
        condition :: Condition
        condition = case relOp of
                    LTH _ -> Slt
                    LE  _ -> Sle
                    GTH _ -> Sgt
                    GE  _ -> Sge
                    EQU _ -> Eq
                    NE _  -> Ne
        op :: Ord b => b -> b -> Bool
        op = case condition of
                Slt -> (<)
                Sle -> (<=)
                Sgt -> (>)
                Sge -> (>=)
                Eq  -> (==)
                Ne  -> (/=)
        icmp :: Value -> Value -> GeneratorStateT Value
        icmp value1 value2 = do
            r <- emitWithGCSE $ Icmp "_" condition value1 value2
            return $ RegisterValue I1 r

generateExpr (EAnd _ e1 e2) = do
    value1 <- generateExpr e1
    case value1 of
        BoolValue True  -> generateExpr e2
        BoolValue False -> return value1
        _               -> do
            pred1 <- gets currentLabel
            labelMid <- freshLabel
            labelEnd <- freshLabel
            emit $ BrConditional value1 labelMid labelEnd
            emit $ Label labelMid
            value2 <- generateExpr e2
            pred2 <- gets currentLabel
            emit $ BrUnconditional labelEnd
            emit $ Label labelEnd
            r <- freshRegister
            emit $ Phi r [(BoolValue False, pred1), (value2, pred2)]
            return $ RegisterValue I1 r

generateExpr (EOr _ e1 e2) = do
    value1 <- generateExpr e1
    case value1 of
        BoolValue True  -> return value1
        BoolValue False -> generateExpr e2
        _               -> do
            pred1 <- gets currentLabel
            labelMid <- freshLabel
            labelEnd <- freshLabel
            emit $ BrConditional value1 labelEnd labelMid
            emit $ Label labelMid
            value2 <- generateExpr e2
            pred2 <- gets currentLabel
            emit $ BrUnconditional labelEnd
            emit $ Label labelEnd
            r <- freshRegister
            emit $ Phi r [(BoolValue True, pred1), (value2, pred2)]
            return $ RegisterValue I1 r

generateIntegerBinaryOperation :: (Name -> Value -> Value -> Instruction) -> (Integer -> Integer -> Integer) -> Expr a -> Expr a -> GeneratorStateT Value
generateIntegerBinaryOperation instruction op arg1 arg2 = do
    value1 <- generateExpr arg1
    value2 <- generateExpr arg2
    case (value1, value2) of
        (IntegerValue n1, IntegerValue n2) -> return $ IntegerValue (op n1 n2)
        _ -> do
            r <- emitWithGCSE $ instruction "_" value1 value2
            return $ RegisterValue I32 r

generateBoolBinaryOperation :: (Name -> Value -> Value -> Instruction) -> (Bool -> Bool -> Bool) -> Expr a -> Expr a -> GeneratorStateT Value
generateBoolBinaryOperation instruction op arg1 arg2 = do
    value1 <- generateExpr arg1
    value2 <- generateExpr arg2
    case (value1, value2) of
        (BoolValue b1, BoolValue b2) -> return $ BoolValue (op b1 b2)
        _ -> do
            r <- emitWithGCSE $ instruction "_" value1 value2
            return $ RegisterValue I1 r


generateBoolExpr :: Expr a -> Label -> Label -> GeneratorStateT ([Label], [Label])

generateBoolExpr e@(EVar _ x) labelTrue labelFalse = do
    label <- gets currentLabel
    value <- generateExpr e
    case value of
        BoolValue True -> do
            emit $ BrUnconditional labelTrue
            return ([label], [])
        BoolValue False -> do
            emit $ BrUnconditional labelFalse
            return ([], [label])
        RegisterValue _ _ -> do
            emit $ BrConditional value labelTrue labelFalse
            return ([label], [label])

generateBoolExpr (ELitTrue _) labelTrue _ = do
    label <- gets currentLabel
    emit $ BrUnconditional labelTrue
    return ([label], [])

generateBoolExpr (ELitFalse _) _ labelFalse = do
    label <- gets currentLabel
    emit $ BrUnconditional labelFalse
    return ([], [label])

generateBoolExpr e@(EApp _ _ _) labelTrue labelFalse = do
    label <- gets currentLabel
    value <- generateExpr e
    emit $ BrConditional value labelTrue labelFalse
    return ([label], [label])

generateBoolExpr (Not _ e) labelTrue labelFalse = generateBoolExpr e labelFalse labelTrue

generateBoolExpr e@(ERel _ _ _ _) labelTrue labelFalse = do
    label <- gets currentLabel
    value <- generateExpr e
    case value of
        BoolValue True -> do
            emit $ BrUnconditional labelTrue
            return ([label], [])
        BoolValue False -> do
            emit $ BrUnconditional labelFalse
            return ([], [label])
        RegisterValue _ _ -> do
            emit $ BrConditional value labelTrue labelFalse
            return ([label], [label])

generateBoolExpr (EAnd _ e1 e2) labelTrue labelFalse = do
    labelMid <- freshLabel
    (midLabels, falseLabels1) <- generateBoolExpr e1 labelMid labelFalse
    case midLabels of
        [] -> return ([], falseLabels1)
        _  -> do
            emit $ Label labelMid
            (trueLabels, falseLabels2) <- generateBoolExpr e2 labelTrue labelFalse
            return (trueLabels, falseLabels1 ++ falseLabels2)

generateBoolExpr (EOr _ e1 e2) labelTrue labelFalse = do
    labelMid <- freshLabel
    (trueLabels1, midLabels) <- generateBoolExpr e1 labelTrue labelMid
    case midLabels of
        [] -> return (trueLabels1, [])
        _  -> do
            emit $ Label labelMid
            (trueLabels2, falseLabels) <- generateBoolExpr e2 labelTrue labelFalse
            return (trueLabels1 ++ trueLabels2, falseLabels)


generateStmt :: Stmt a -> GeneratorStateT ()

generateStmt (Empty _) = return ()

generateStmt (BStmt _ (Block _ stmts)) = do
    variablesBefore <- gets variables
    mapM_ generateStmt stmts
    variablesAfter <- gets variables
    setVariables $ Map.intersectionWith merge2 variablesAfter variablesBefore
    where
        merge2 :: VariableValues -> VariableValues -> VariableValues
        merge2 valuesAfter valuesBefore
            | stackSize valuesBefore < stackSize valuesAfter = valuesAfter { stack = tail $ stack valuesAfter }
            | otherwise                                      = valuesAfter

generateStmt (Decl location t items) =
    mapM_ generateItem items
    where
        generateItem (NoInit location x) = generateItem $ Init location x defaultExpr
        generateItem (Init _ x e) = do
            value <- generateExpr e
            modify $ \state -> state { variables = Map.insertWith push x (VariableValues [value] 1) $ variables state }
        defaultExpr =
            case t of
                Int _  -> ELitInt location 0
                Str _  -> EString location ""
                Bool _ -> ELitFalse location
        push :: VariableValues -> VariableValues -> VariableValues
        push (VariableValues [newValue] _) values = values { stack = newValue : stack values, stackSize = stackSize values + 1 }

generateStmt (Ass _ x e) = do
    value <- generateExpr e
    changeVariableValue x value

generateStmt (Incr location x) = generateStmt $ Ass location x $ EAdd location (EVar location x) (Plus location) (ELitInt location 1)

generateStmt (Decr location x) = generateStmt $ Ass location x $ EAdd location (EVar location x) (Minus location) (ELitInt location 1)

generateStmt (Ret _ e) = do
    value <- generateExpr e
    emit $ RetValue value

generateStmt (VRet _) = emit RetVoid

generateStmt (Cond _ conditionExpr thenStmt) = do
    labelThen <- freshLabel
    labelEnd <- freshLabel
    (thenLabels, predsBegin) <- generateBoolExpr conditionExpr labelThen labelEnd
    let (generateThen, generateEnd) = case (thenLabels, predsBegin) of
                                        ([], _) -> (False, False)
                                        (_, []) -> (True, False)
                                        _       -> (True, True)
    if not generateThen
    then
        emit $ Label labelEnd
    else do
        variablesBegin <- gets $ Map.toAscList . variables
        resultsBegin <- gets instructionResults
        emit $ Label labelThen
        generateStmt thenStmt
        when generateEnd $ do
            predThen <- gets currentLabel
            variablesThen <- gets $ Map.toAscList . variables
            emit $ BrUnconditional labelEnd
            emit $ Label labelEnd
            variablesEnd <- zipWithM (merge2 predsBegin predThen) variablesBegin variablesThen
            modify $ \state -> state { variables = Map.fromAscList variablesEnd, instructionResults = resultsBegin }
    where
        merge2 :: [Label] -> Label -> (Ident, VariableValues) -> (Ident, VariableValues) -> GeneratorStateT (Ident, VariableValues)
        merge2 predsBegin predThen (x, valuesBegin) (_, valuesThen) =
            let
                valueBegin = top valuesBegin
                valueThen  = top valuesThen
            in
            if valueBegin == valueThen
            then
                return (x, valuesBegin)
            else do
                r <- emitWithGCSE $ Phi "_" $ sort $ (valueThen, predThen) : map ((,) valueBegin) predsBegin
                return (x, changeTop (RegisterValue (tyOfValue valueBegin) r) valuesBegin)

generateStmt (CondElse _ conditionExpr thenStmt elseStmt) = do
    labelThen <- freshLabel
    labelElse <- freshLabel
    labelEnd <- freshLabel
    variablesBegin <- gets variables
    resultsBegin <- gets instructionResults
    (thenLabels, elseLabels) <- generateBoolExpr conditionExpr labelThen labelElse
    let (generateThen, generateElse) = case (thenLabels, elseLabels) of
                                        ([], _) -> (False, True)
                                        (_, []) -> (True, False)
                                        _       -> (True, True)
    when generateThen $ do
        emit $ Label labelThen
        generateStmt thenStmt
        emit $ BrUnconditional labelEnd
    predThen <- gets currentLabel
    variablesThen <- gets $ Map.toAscList . variables
    when generateElse $ do
        emit $ Label labelElse
        modify $ \state -> state { variables = variablesBegin, instructionResults = resultsBegin }
        generateStmt elseStmt
        emit $ BrUnconditional labelEnd
    predElse <- gets currentLabel
    emit $ Label labelEnd
    when (generateThen && generateElse) $ do
        variablesElse <- gets $ Map.toAscList . variables
        variablesEnd <- sequence $ zipWith3 (merge3 predThen predElse) (Map.toAscList variablesBegin) variablesThen variablesElse
        modify $ \state -> state { variables = Map.fromAscList variablesEnd, instructionResults = resultsBegin }
    where
        merge3 :: Label -> Label -> (Ident, VariableValues) -> (Ident, VariableValues) -> (Ident, VariableValues) -> GeneratorStateT (Ident, VariableValues)
        merge3 predThen predElse (x, valuesBegin) (_, valuesThen) (_, valuesElse) =
            let
                valueBegin = top valuesBegin
                valueThen  = top valuesThen
                valueElse  = top valuesElse
            in
            if valueBegin == valueThen && valueThen == valueElse
            then
                return (x, valuesBegin)
            else do
                r <- emitWithGCSE $ Phi "_" $ sort [(valueThen, predThen), (valueElse, predElse)]
                return (x, changeTop (RegisterValue (tyOfValue valueBegin) r) valuesBegin)

generateStmt (While _ conditionExpr bodyStmt) = do
    state <- get
    unless (evalState (generateExpr conditionExpr) state == BoolValue False) $ do
        predBegin <- gets currentLabel
        labelBody <- freshLabel
        labelCondition <- freshLabel
        labelEnd <- freshLabel
        variablesBegin <- gets $ Map.toAscList . variables
        resultsBegin <- gets instructionResults
        variablesBeginRegisters <- mapM toRegisterValue variablesBegin
        state' <- gets $ \state -> state { variables = Map.fromAscList variablesBeginRegisters }
        variablesBodyBefore <- sequence $ zipWith3 mergeBefore variablesBeginRegisters (Map.toAscList . variables . flip execState state' $ generateStmt bodyStmt) variablesBegin
        emit $ BrUnconditional labelCondition
        emit $ Label labelBody
        setVariables $ Map.fromAscList variablesBodyBefore
        generateStmt bodyStmt
        predBody <- gets currentLabel
        variablesBodyAfter <- gets $ Map.toAscList . variables
        emit $ BrUnconditional labelCondition
        emit $ Label labelCondition
        setVariables $ Map.fromAscList variablesBodyBefore
        sequence_ $ zipWith3 (mergeAfter predBegin predBody) variablesBegin variablesBodyAfter variablesBodyBefore
        generateBoolExpr conditionExpr labelBody labelEnd
        emit $ Label labelEnd
        modify $ \state -> state { instructionResults = resultsBegin }
    where
        toRegisterValue :: (Ident, VariableValues) -> GeneratorStateT (Ident, VariableValues)
        toRegisterValue (x, values) =
            case top values of
                RegisterValue _ _ -> return (x, values)
                _ -> do
                    r <- freshRegister
                    return (x, changeTop (RegisterValue (tyOfValue $ top values) r) values)
        mergeBefore :: (Ident, VariableValues) -> (Ident, VariableValues) -> (Ident, VariableValues) -> GeneratorStateT (Ident, VariableValues)
        mergeBefore (x, valuesBeginRegister) (_, valuesBody) (_, valuesBegin)
            | top valuesBeginRegister == top valuesBody  = return (x, valuesBegin)
            | top valuesBeginRegister /= top valuesBegin = return (x, valuesBeginRegister)
            | otherwise = do
                r <- freshRegister
                return (x, changeTop (RegisterValue (tyOfValue $ top valuesBeginRegister) r) valuesBeginRegister)
        mergeAfter :: Label -> Label -> (Ident, VariableValues) -> (Ident, VariableValues) -> (Ident, VariableValues) -> GeneratorStateT ()
        mergeAfter predBegin predBody (_, valuesBegin) (_, valuesBodyAfter) (_, valuesBodyBefore) =
            let
                valueBegin = top valuesBegin
                valueBodyAfter = top valuesBodyAfter
            in
            unless (valueBegin == valueBodyAfter) $ do
                let RegisterValue _ r = top valuesBodyBefore
                emit $ Phi r [(valueBegin, predBegin), (valueBodyAfter, predBody)]

generateStmt (SExp _ e) = generateExpr e >> return ()


generateTopDef :: Map.Map Ident Ty -> Map.Map String Name -> TopDef a -> Function
generateTopDef functions stringLiterals (FnDef _ returnType (Ident ident) args (Block _ stmts)) =
    optimizeFunction $ Function ("@" ++ ident) returnTy (map snd argList) basicBlocks
    where
        instructionList :: [Instruction]
        instructionList = code . flip execState initialState $ mapM_ generateStmt stmts
        basicBlocks :: [BasicBlock]
        basicBlocks = snd $ foldl' instructionToBasicBlockAcc (BasicBlock "_" [] [] (if returnTy == VoidTy then RetVoid else Unreachable), []) instructionList
        instructionToBasicBlockAcc :: (BasicBlock, [BasicBlock]) -> Instruction -> (BasicBlock, [BasicBlock])
        instructionToBasicBlockAcc (block, blocks) (Label l) = (BasicBlock "_" [] [] (if returnTy == VoidTy then RetVoid else Unreachable), block { label = l } : blocks)
        instructionToBasicBlockAcc (block, blocks) instruction
            | isTerminator instruction = (block { terminator = instruction }, blocks)
            | otherwise                = (block { instructions = instruction : instructions block }, blocks)
        isTerminator :: Instruction -> Bool
        isTerminator RetVoid               = True
        isTerminator (RetValue _)          = True
        isTerminator (BrConditional _ _ _) = True
        isTerminator (BrUnconditional _)   = True
        isTerminator Unreachable           = True
        isTerminator _                     = False
        initialState :: CodeGenerationState
        initialState = CodeGenerationState 0 0 "%entry" variables functions stringLiterals [Label "%entry"] Map.empty
        variables :: Map.Map Ident VariableValues
        variables = Map.fromList $ map (\(ident, value) -> (ident, VariableValues [value] 1)) argList
        argList :: [(Ident, Value)]
        argList = map (\(Arg _ t ident@(Ident name)) -> (ident, RegisterValue (typeToTy t) ("%" ++ name))) args
        returnTy :: Ty
        returnTy = typeToTy returnType


generateCode :: Program a -> Map.Map Ident (Type a) -> String
generateCode program@(Program _ topDefs) funs =
    showString declarations . showStringLiterals . showChar '\n' . showFunctions $ ""
    where
        showFunctions :: ShowS
        showFunctions = foldr ((.) . shows) id generatedFunctions
        showStringLiterals :: ShowS
        showStringLiterals = Map.foldrWithKey (\str name -> (.) (showString $ showStringLiteral str name)) id stringLiterals
        generatedFunctions :: [Function]
        generatedFunctions = map (generateTopDef functions stringLiterals) topDefs
        functions :: Map.Map Ident Ty
        functions = Map.map (\(Fun _ returnType _) -> typeToTy returnType) funs
        stringLiterals :: Map.Map String Name
        stringLiterals = definedStringLiterals program
        showStringLiteral :: String -> Name -> String
        showStringLiteral str name = unwords [name, "= private constant [", show $ length str + 1, "x i8] c\"" ++ str ++ "\\00\"\n"]
