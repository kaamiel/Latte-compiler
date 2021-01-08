module Backend.CodeGenerator (generateCode) where

import qualified Data.Map as Map
import Data.List (foldl')
import Data.Maybe
import Control.Monad.State
import AbsLatte
import Backend.LLVMAsm


data CodeGenerationState = CodeGenerationState
    { nextRegisterNumber      :: Int
    , nextLabelNumber         :: Int
    , currentLabel            :: Label
    , variables               :: Map.Map Ident Value
    , functions               :: Map.Map Ident (Ty, [Ty])
    , stringLiterals          :: Map.Map String Name
    , code                    :: [Instruction]
    }

type GeneratorStateT = State CodeGenerationState


prologue :: String
prologue =
    "declare void @printInt(i32)\n\
    \declare void @printString(i8*)\n\
    \declare void @error()\n\
    \declare i32  @readInt()\n\
    \declare i8*  @readString()\n\
    \declare i8*  @concatStrings(i8*, i8*)\n\n"

emptyName :: Name
emptyName = "_"

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

generateExpr (EVar _ x) = do
    value <- gets $ fromJust . Map.lookup x . variables
    case value of
        RegisterValue (Ptr ty) r -> do
            ret <- freshRegister
            emit $ Load ret value
            return $ RegisterValue ty ret
        _ -> return value

generateExpr (ELitInt _ n) = return $ IntegerValue n

generateExpr (ELitTrue _) = return $ BoolValue True

generateExpr (ELitFalse _) = return $ BoolValue False

generateExpr (EApp location f@(Ident ident) args) = do
    (returnTy, _) <- gets $ fromJust . Map.lookup f . functions
    argValues <- mapM generateExpr args
    if returnTy == VoidTy
    then do
        emit $ CallVoid ("@" ++ ident) argValues
        return NoValue
    else do
        ret <- freshRegister
        emit $ Call ret returnTy ("@" ++ ident) argValues
        return $ RegisterValue returnTy ret

generateExpr (EString _ string) = do
    let string' = if length string < 2 then string else init . tail $ string
    ret <- freshRegister
    name <- gets $ fromJust . Map.lookup string' . stringLiterals
    emit $ Bitcast ret (length string' + 1) name
    return $ RegisterValue (Ptr I8) ret

generateExpr (Neg location e) = generateExpr $ EAdd location (ELitInt location 0) (Minus location) e

generateExpr (Not location e) = generateBinaryOperation Xor e (ELitTrue location)

generateExpr (EMul _ e1 (Times _) e2) = generateBinaryOperation Mul e1 e2

generateExpr (EMul _ e1 (Div _) e2) = generateBinaryOperation Sdiv e1 e2

generateExpr (EMul _ e1 (Mod _) e2) = generateBinaryOperation Srem e1 e2

generateExpr (EAdd _ e1 (Plus _) e2) = do
    value1 <- generateExpr e1
    value2 <- generateExpr e2
    ret <- freshRegister
    case value1 of
        RegisterValue (Ptr I8) _ -> do
            emit $ Call ret (Ptr I8) "@concatStrings" [value1, value2]
            return $ RegisterValue (Ptr I8) ret
        _ -> do
            emit $ Add ret value1 value2
            return $ RegisterValue I32 ret

generateExpr (EAdd _ e1 (Minus _) e2) = generateBinaryOperation Sub e1 e2

generateExpr (ERel _ e1 relOp e2) = do
    value1 <- generateExpr e1
    value2 <- generateExpr e2
    ret <- freshRegister
    let condition = case relOp of
                        LTH _ -> Slt
                        LE  _ -> Sle
                        GTH _ -> Sgt
                        GE  _ -> Sge
                        EQU _ -> Eq
                        NE _  -> Ne
    emit $ Icmp ret condition value1 value2
    return $ RegisterValue I1 ret

generateExpr (EAnd _ e1 e2) = do
    value1 <- generateExpr e1
    pred1 <- gets currentLabel
    labelMid <- freshLabel
    labelEnd <- freshLabel
    emit $ BrConditional value1 labelMid labelEnd
    emit $ Label labelMid
    value2 <- generateExpr e2
    pred2 <- gets currentLabel
    emit $ BrUnconditional labelEnd
    emit $ Label labelEnd
    ret <- freshRegister
    emit $ Phi ret [(BoolValue False, pred1), (value2, pred2)]
    return $ RegisterValue I1 ret

generateExpr (EOr _ e1 e2) = do
    value1 <- generateExpr e1
    pred1 <- gets currentLabel
    labelMid <- freshLabel
    labelEnd <- freshLabel
    emit $ BrConditional value1 labelEnd labelMid
    emit $ Label labelMid
    value2 <- generateExpr e2
    pred2 <- gets currentLabel
    emit $ BrUnconditional labelEnd
    emit $ Label labelEnd
    ret <- freshRegister
    emit $ Phi ret [(BoolValue True, pred1), (value2, pred2)]
    return $ RegisterValue I1 ret

generateBinaryOperation :: (Name -> Value -> Value -> Instruction) -> Expr a -> Expr a -> GeneratorStateT Value
generateBinaryOperation instruction arg1 arg2 = do
    value1 <- generateExpr arg1
    value2 <- generateExpr arg2
    ret <- freshRegister
    emit $ instruction ret value1 value2
    return $ RegisterValue (tyOfValue value1) ret


generateStmt :: Stmt a -> GeneratorStateT ()

generateStmt (Empty _) = return ()

generateStmt (BStmt _ (Block _ stmts)) = do
    variables' <- gets variables
    mapM_ generateStmt stmts
    modify $ \state -> state { variables = variables' }

generateStmt (Decl location t items) =
    mapM_ generateItem items
    where
        ty = typeToTy t
        defaultValue = case ty of
                            I32    -> ELitInt location 0
                            Ptr I8 -> EString location ""
                            I1     -> ELitFalse location
        generateItem (NoInit location x) = generateItem (Init location x defaultValue)
        generateItem (Init _ x e) = do
            eValue <- generateExpr e
            name <- freshRegister
            let xValue = RegisterValue (Ptr ty) name
            modify $ \state -> state { variables = Map.insert x xValue $ variables state }
            emit $ Alloca name ty
            emit $ Store eValue xValue

generateStmt (Ass _ x e) = do
    eValue <- generateExpr e
    xValue <- gets $ fromJust . Map.lookup x . variables
    case xValue of
        RegisterValue (Ptr ty) r -> emit $ Store eValue xValue

generateStmt (Incr location x) = generateStmt $ Ass location x $ EAdd location (EVar location x) (Plus location) (ELitInt location 1)

generateStmt (Decr location x) = generateStmt $ Ass location x $ EAdd location (EVar location x) (Minus location) (ELitInt location 1)

generateStmt (Ret _ e) = do
    value <- generateExpr e
    emit $ RetValue value

generateStmt (VRet _) = emit RetVoid

generateStmt (Cond _ conditionExpr thenStmt) = do
    value <- generateExpr conditionExpr
    labelTrue <- freshLabel
    labelEnd <- freshLabel
    emit $ BrConditional value labelTrue labelEnd
    emit $ Label labelTrue
    generateStmt thenStmt
    emit $ BrUnconditional labelEnd
    emit $ Label labelEnd

generateStmt (CondElse _ conditionExpr thenStmt elseStmt) = do
    value <- generateExpr conditionExpr
    labelTrue <- freshLabel
    labelFalse <- freshLabel
    labelEnd <- freshLabel
    emit $ BrConditional value labelTrue labelFalse
    emit $ Label labelTrue
    generateStmt thenStmt
    emit $ BrUnconditional labelEnd
    emit $ Label labelFalse
    generateStmt elseStmt
    emit $ BrUnconditional labelEnd
    emit $ Label labelEnd

generateStmt (While _ conditionExpr bodyStmt) = do
    labelBody <- freshLabel
    labelCondition <- freshLabel
    labelEnd <- freshLabel
    emit $ BrUnconditional labelCondition
    emit $ Label labelBody
    generateStmt bodyStmt
    emit $ BrUnconditional labelCondition
    emit $ Label labelCondition
    value <- generateExpr conditionExpr
    emit $ BrConditional value labelBody labelEnd
    emit $ Label labelEnd

generateStmt (SExp _ e) = do
    generateExpr e
    return ()


generateTopDef :: Map.Map Ident (Ty, [Ty]) -> Map.Map String Name -> TopDef a -> Function
generateTopDef functions stringLiterals (FnDef _ returnType (Ident ident) args (Block _ stmts)) =
    Function ("@" ++ ident) returnTy (map snd argNamesAndTys) basicBlocks
    where
        instructionList :: [Instruction]
        instructionList = code . flip execState initialState $ mapM_ generateStmt stmts
        basicBlocks :: [BasicBlock]
        basicBlocks = snd $ foldl' instructionToBasicBlockAcc (BasicBlock emptyName [] [] (if returnTy == VoidTy then RetVoid else Unreachable), []) instructionList
        instructionToBasicBlockAcc :: (BasicBlock, [BasicBlock]) -> Instruction -> (BasicBlock, [BasicBlock])
        instructionToBasicBlockAcc (block, blocks) (Label l) = (BasicBlock emptyName [] [] (if returnTy == VoidTy then RetVoid else Unreachable), block { label = l } : blocks)
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
        initialState = CodeGenerationState 0 0 "%entry" variables functions stringLiterals initialInstructions
        variables :: Map.Map Ident Value
        variables = Map.fromList $ map argNameAndTyToVariable argNamesAndTys
        argNamesAndTys :: [(Ident, (Ty, Name))]
        argNamesAndTys = map argNameAndTy args
        argNameAndTy :: Arg a -> (Ident, (Ty, Name))
        argNameAndTy (Arg _ t name@(Ident ident)) = (name, (typeToTy t, "%" ++ ident))
        argNameAndTyToVariable :: (Ident, (Ty, Name)) -> (Ident, Value)
        argNameAndTyToVariable (name, (ty, r)) = (name, RegisterValue (Ptr ty) (r ++ "_ptr"))
        initialInstructions :: [Instruction]
        initialInstructions = foldr (\(_, (ty, r)) acc -> Store (RegisterValue ty r) (RegisterValue (Ptr ty) $ r ++ "_ptr") : Alloca (r ++ "_ptr") ty : acc) [Label "%entry"] argNamesAndTys
        returnTy :: Ty
        returnTy = typeToTy returnType


generateCode :: Program a -> Map.Map Ident (Type a) -> String
generateCode program@(Program _ topDefs) funs =
    showString prologue . showStringLiterals . showChar '\n' . showFunctions $ "\n"
    where
        showFunctions :: ShowS
        showFunctions = foldr (\f -> (.) (shows f)) id generatedFunctions
        showStringLiterals :: ShowS
        showStringLiterals = Map.foldrWithKey (\str name -> (.) (showString $ showStringLiteral str name)) id stringLiterals
        generatedFunctions :: [Function]
        generatedFunctions = map (generateTopDef functions stringLiterals) topDefs
        functions :: Map.Map Ident (Ty, [Ty])
        functions = Map.map funTypeToTy funs
        funTypeToTy :: Type a -> (Ty, [Ty])
        funTypeToTy (Fun _ returnType argTypes) = (typeToTy returnType, map typeToTy argTypes)
        stringLiterals :: Map.Map String Name
        stringLiterals = definedStringLiterals program
        showStringLiteral :: String -> Name -> String
        showStringLiteral str name = unwords [name, "= private constant [", show $ length str + 1, "x i8] c\"" ++ str ++ "\\00\"\n"]

