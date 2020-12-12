module Frontend.ConstExprOptimizer where

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


optimizeStmt :: Stmt a -> Stmt a

optimizeStmt (BStmt location1 (Block location2 stmts)) = BStmt location1 (Block location2 $ map optimizeStmt stmts)

optimizeStmt (Decl location t items) =
    Decl location t $ map optimizeItem items
    where
        optimizeItem :: Item a -> Item a
        optimizeItem (Init l x e) = Init l x $ optimizeExpr e
        optimizeItem noInitItem   = noInitItem

optimizeStmt (Ass location x e) = Ass location x $ optimizeExpr e

optimizeStmt (Ret location e) = Ret location $ optimizeExpr e

optimizeStmt (Cond location conditionExpr thenStmt) =
    case optimizedConditionExpr of
        ELitTrue _  -> optimizedThenStmt
        ELitFalse _ -> Empty location
        _           -> Cond location optimizedConditionExpr optimizedThenStmt
    where
        optimizedConditionExpr = optimizeExpr conditionExpr
        optimizedThenStmt      = optimizeStmt thenStmt

optimizeStmt (CondElse location conditionExpr thenStmt elseStmt) =
    case optimizedConditionExpr of
        ELitTrue _  -> optimizedThenStmt
        ELitFalse _ -> optimizedElseStmt
        _           -> CondElse location optimizedConditionExpr optimizedThenStmt optimizedElseStmt
    where
        optimizedConditionExpr = optimizeExpr conditionExpr
        optimizedThenStmt      = optimizeStmt thenStmt
        optimizedElseStmt      = optimizeStmt elseStmt

optimizeStmt (While location conditionExpr bodyStmt) =
    case optimizedConditionExpr of
        ELitFalse _ -> Empty location
        _           -> While location optimizedConditionExpr optimizedBodyStmt
    where
        optimizedConditionExpr = optimizeExpr conditionExpr
        optimizedBodyStmt      = optimizeStmt bodyStmt

optimizeStmt (SExp location e) = SExp location $ optimizeExpr e

optimizeStmt stmt = stmt


optimizeProgram :: Program a -> Program a
optimizeProgram (Program location topDefs) =
    Program location (map optimizeTopDef topDefs)
    where
        optimizeTopDef :: TopDef a -> TopDef a
        optimizeTopDef (FnDef location returnType name args (Block location2 stmts)) = FnDef location returnType name args (Block location2 $ map optimizeStmt stmts)
