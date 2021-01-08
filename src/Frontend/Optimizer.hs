module Frontend.Optimizer where

import AbsLatte
import Frontend.Utils


isIntConstExpr :: Expr a -> Bool
isIntConstExpr (ELitInt _ _)    = True
isIntConstExpr (Neg _ e)        = isIntConstExpr e
isIntConstExpr (EMul _ e1 _ e2) = isIntConstExpr e1 && isIntConstExpr e2
isIntConstExpr (EAdd _ e1 _ e2) = isIntConstExpr e1 && isIntConstExpr e2
isIntConstExpr _                = False

isBoolConstExpr :: Expr a -> Bool
isBoolConstExpr (ELitTrue _)     = True
isBoolConstExpr (ELitFalse _)    = True
isBoolConstExpr (Not _ e)        = isBoolConstExpr e
isBoolConstExpr (ERel _ e1 _ e2) = (isIntConstExpr e1 && isIntConstExpr e2) || (isBoolConstExpr e1 && isBoolConstExpr e2)
isBoolConstExpr (EAnd _ e1 e2)   = (isBoolConstExpr e1 && not (evalBoolConstExpr e1)) || (isBoolConstExpr e1 && isBoolConstExpr e2)
isBoolConstExpr (EOr _ e1 e2)    = (isBoolConstExpr e1 && evalBoolConstExpr e1) || (isBoolConstExpr e1 && isBoolConstExpr e2)
isBoolConstExpr _                = False

evalIntConstExpr :: Expr a -> Integer
evalIntConstExpr (ELitInt _ n)            = n
evalIntConstExpr (Neg _ e)                = -(evalIntConstExpr e)
evalIntConstExpr (EMul _ e1 (Times _) e2) = (evalIntConstExpr e1) * (evalIntConstExpr e2)
evalIntConstExpr (EMul _ e1 (Div _) e2)   = (evalIntConstExpr e1) `div` (evalIntConstExpr e2)
evalIntConstExpr (EMul _ e1 (Mod _) e2)   = (evalIntConstExpr e1) `mod` (evalIntConstExpr e2)
evalIntConstExpr (EAdd _ e1 (Plus _) e2)  = (evalIntConstExpr e1) + (evalIntConstExpr e2)
evalIntConstExpr (EAdd _ e1 (Minus _) e2) = (evalIntConstExpr e1) - (evalIntConstExpr e2)

evalBoolConstExpr :: Expr a -> Bool
evalBoolConstExpr (ELitTrue _)             = True
evalBoolConstExpr (ELitFalse _)            = False
evalBoolConstExpr (Not _ e)                = not $ evalBoolConstExpr e
evalBoolConstExpr (ERel l1 e1 (LTH l2) e2) = evalBoolConstExpr (ERel l1 e1 (LE l2) e2) && evalBoolConstExpr (ERel l1 e1 (NE l2) e2)
evalBoolConstExpr (ERel _ e1 (LE _) e2)    = if isIntConstExpr e1 && isIntConstExpr e2 then evalIntConstExpr e1 <= evalIntConstExpr e2 else evalBoolConstExpr e1 <= evalBoolConstExpr e2
evalBoolConstExpr (ERel l1 e1 (GTH l2) e2) = evalBoolConstExpr (ERel l1 e2 (LTH l2) e1)
evalBoolConstExpr (ERel l1 e1 (GE l2) e2)  = evalBoolConstExpr (ERel l1 e2 (LE l2) e1)
evalBoolConstExpr (ERel _ e1 (EQU _) e2)   = if isIntConstExpr e1 && isIntConstExpr e2 then evalIntConstExpr e1 == evalIntConstExpr e2 else evalBoolConstExpr e1 == evalBoolConstExpr e2
evalBoolConstExpr (ERel l1 e1 (NE l2) e2)  = not $ evalBoolConstExpr (ERel l1 e1 (EQU l2) e2)
evalBoolConstExpr (EAnd _ e1 e2)           = (evalBoolConstExpr e1) && (evalBoolConstExpr e2)
evalBoolConstExpr (EOr _ e1 e2)            = (evalBoolConstExpr e1) || (evalBoolConstExpr e2)


optimizeExpr :: Expr a -> Expr a
optimizeExpr e
    | isIntConstExpr e  = ELitInt (sourceLocationOfExpr e) (evalIntConstExpr e)
    | isBoolConstExpr e = (if evalBoolConstExpr e then ELitTrue else ELitFalse) (sourceLocationOfExpr e)
    | otherwise         = e


reachable :: (Stmt a, Int) -> Bool
reachable (BStmt _ _, k)        = k <= 1
reachable (Ret _ _, k)          = k <= 1
reachable (VRet _, k)           = k <= 1
reachable (Cond _ _ _, k)       = k <= 1
reachable (CondElse _ _ _ _, k) = k <= 1
reachable (While _ _ _, k)      = k <= 1
reachable (_, k)                = k < 1


optimizeStmtAcc :: (Stmt a, Int) -> Stmt a -> (Stmt a, Int)

optimizeStmtAcc (_, k) (BStmt location1 (Block location2 stmts)) =
    (BStmt location1 (Block location2 optimizedStmts), k')
    where
        reachableStmtsAndKs = takeWhile reachable $ scanl optimizeStmtAcc (Empty location1, k) stmts
        getStmtsAndK :: [(Stmt a, Int)] -> ([Stmt a], Int)
        getStmtsAndK [] = ([], k)
        getStmtsAndK prefix = (map fst prefix, snd $ last prefix)
        (optimizedStmts, k') = getStmtsAndK reachableStmtsAndKs

optimizeStmtAcc (_, k) (Decl location t items) =
    (Decl location t $ map optimizeItem items, k)
    where
        optimizeItem :: Item a -> Item a
        optimizeItem (Init l x e) = Init l x $ optimizeExpr e
        optimizeItem noInitItem   = noInitItem

optimizeStmtAcc (_, k) (Ass location x e) = (Ass location x $ optimizeExpr e, k)

optimizeStmtAcc (_, k) (Ret location e) = (Ret location $ optimizeExpr e, k + 1)

optimizeStmtAcc (_, k) (VRet location) = (VRet location, k + 1)

optimizeStmtAcc (_, k) (Cond location conditionExpr thenStmt) =
    case optimizedConditionExpr of
        ELitTrue _  -> (optimizedThenStmt, k')
        ELitFalse _ -> (Empty location, k)
        _           -> (Cond location optimizedConditionExpr optimizedThenStmt, k)
    where
        optimizedConditionExpr  = optimizeExpr conditionExpr
        (optimizedThenStmt, k') = optimizeStmtAcc (thenStmt, k) thenStmt

optimizeStmtAcc (_, k) (CondElse location conditionExpr thenStmt elseStmt) =
    case optimizedConditionExpr of
        ELitTrue _  -> (optimizedThenStmt, k1')
        ELitFalse _ -> (optimizedElseStmt, k2')
        _           -> (CondElse location optimizedConditionExpr optimizedThenStmt optimizedElseStmt, min k1' k2')
    where
        optimizedConditionExpr   = optimizeExpr conditionExpr
        (optimizedThenStmt, k1') = optimizeStmtAcc (thenStmt, k) thenStmt
        (optimizedElseStmt, k2') = optimizeStmtAcc (elseStmt, k) elseStmt

optimizeStmtAcc (_, k) (While location conditionExpr bodyStmt) =
    case optimizedConditionExpr of
        ELitFalse _ -> (Empty location, k)
        ELitTrue _  -> (While location optimizedConditionExpr optimizedBodyStmt, 1)
        _           -> (While location optimizedConditionExpr optimizedBodyStmt, k)
    where
        optimizedConditionExpr  = optimizeExpr conditionExpr
        (optimizedBodyStmt, _) = optimizeStmtAcc (bodyStmt, k) bodyStmt

optimizeStmtAcc (_, k) (SExp location e) = (SExp location $ optimizeExpr e, k)

optimizeStmtAcc (_, k) stmt = (stmt, k)


optimizeProgram :: Program a -> Program a
optimizeProgram (Program location topDefs) =
    Program location (map optimizeTopDef topDefs)
    where
        optimizeTopDef (FnDef location returnType name args (Block location2 stmts)) = FnDef location returnType name args (Block location2 $ optimizeStmts stmts)
        optimizeStmts = map fst . takeWhile reachable . scanl optimizeStmtAcc (Empty location, 0)
