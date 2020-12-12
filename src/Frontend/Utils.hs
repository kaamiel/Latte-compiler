module Frontend.Utils where

import AbsLatte


type SourceLocation = Maybe (Int, Int)

sourceLocationOfExpr :: Expr a -> a
sourceLocationOfExpr e =
    case e of
        EVar l _     -> l
        ELitInt l _  -> l
        ELitTrue l   -> l
        ELitFalse l  -> l
        EApp l _ _   -> l
        EString l _  -> l
        Neg l _      -> l
        Not l _      -> l
        EMul l _ _ _ -> l
        EAdd l _ _ _ -> l
        ERel l _ _ _ -> l
        EAnd l _ _   -> l
        EOr l _ _    -> l

sourceLocationOfStmt :: Stmt a -> a
sourceLocationOfStmt stmt =
    case stmt of
        Empty l          -> l
        BStmt l _        -> l
        Decl l _ _       -> l
        Ass l _ _        -> l
        Incr l _         -> l
        Decr l _         -> l
        Ret l _          -> l
        VRet l           -> l
        Cond l _ _       -> l
        CondElse l _ _ _ -> l
        While l _ _      -> l
        SExp l _         -> l

sourceLocationOfBlocksLastStatement :: Block a -> a
sourceLocationOfBlocksLastStatement (Block location [])    = location
sourceLocationOfBlocksLastStatement (Block location stmts) = sourceLocationOfStmt $ last stmts

voidType :: Type a -> Bool
voidType (Void _) = True
voidType _        = False

compatibleNonVoidTypes :: Type a -> Type a -> Bool
compatibleNonVoidTypes (Int _) (Int _)   = True
compatibleNonVoidTypes (Str _) (Str _)   = True
compatibleNonVoidTypes (Bool _) (Bool _) = True
compatibleNonVoidTypes _ _               = False
