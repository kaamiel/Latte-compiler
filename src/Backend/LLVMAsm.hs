module Backend.LLVMAsm where

import Data.List (intercalate)
import AbsLatte


type Name = String
type Label = String


data Ty
    = VoidTy
    | I1
    | I8
    | I32
    | Ptr Ty
  deriving (Eq, Ord)

instance Show Ty where
    show VoidTy   = "void"
    show I1       = "i1"
    show I8       = "i8"
    show I32      = "i32"
    show (Ptr ty) = show ty ++ "*"


data Value
    = NoValue
    | RegisterValue Ty Name
    | IntegerValue Integer
    | BoolValue Bool
  deriving (Eq, Ord)

instance Show Value where
    show NoValue             = ""
    show (RegisterValue _ r) = r
    show (IntegerValue n)    = show n
    show (BoolValue b)       = if b then "true" else "false"


data Condition = Eq | Ne | Sgt | Sge | Slt | Sle deriving (Eq, Ord)

instance Show Condition where
    show Eq  = "eq"
    show Ne  = "ne"
    show Sgt = "sgt"
    show Sge = "sge"
    show Slt = "slt"
    show Sle = "sle"


data Instruction
    = Label Label
    | Add Name Value Value
    | Sub Name Value Value
    | Mul Name Value Value
    | Sdiv Name Value Value
    | Srem Name Value Value
    | Xor Name Value Value
    | RetVoid
    | RetValue Value
    | Icmp Name Condition Value Value
    | BrConditional Value Label Label
    | BrUnconditional Label
    | CallVoid Name [Value]
    | Call Name Ty Name [Value]
    | Phi Name [(Value, Label)]
    | Bitcast Name Int Name
    | Unreachable
  deriving Ord

instance Show Instruction where
    show (Label label)                              = tail label ++ ":"
    show (Add result op1 op2)                       = unwords [result, "= add", showTyAndValue op1, ",", show op2]
    show (Sub result op1 op2)                       = unwords [result, "= sub", showTyAndValue op1, ",", show op2]
    show (Mul result op1 op2)                       = unwords [result, "= mul", showTyAndValue op1, ",", show op2]
    show (Sdiv result op1 op2)                      = unwords [result, "= sdiv", showTyAndValue op1, ",", show op2]
    show (Srem result op1 op2)                      = unwords [result, "= srem", showTyAndValue op1, ",", show op2]
    show (Xor result op1 op2)                       = unwords [result, "= xor", showTyAndValue op1, ",", show op2]
    show RetVoid                                    = "ret void"
    show (RetValue value)                           = unwords ["ret", showTyAndValue value]
    show (Icmp result contidion op1 op2)            = unwords [result, "= icmp", show contidion, showTyAndValue op1, ",", show op2]
    show (BrConditional cond labelTrue labelFalse)  = unwords ["br i1", show cond, ", label", labelTrue, ", label", labelFalse]
    show (BrUnconditional label)                    = unwords ["br label", label]
    show (CallVoid name args)                       = unwords ["call void", name, "(", intercalate ", " (map showTyAndValue args), ")"]
    show (Call result ty name args)                 = unwords [result, "= call", show ty, name, "(", intercalate ", " (map showTyAndValue args), ")"]
    show (Phi result values)                        =
        unwords [result, "= phi", show . tyOfValue . fst . head $ values, intercalate ", " (map showValueAndLabel values)]
        where
            showValueAndLabel (value, label) = "[" ++ show value ++ ", " ++ label ++ "]"
    show (Bitcast result n pointer)                 = unwords [result, "= bitcast [", show n, "x i8 ]*", pointer, "to i8*"]
    show Unreachable                                = "unreachable"

instance Eq Instruction where
    Add _ op11 op21             == Add _ op12 op22             = op11 == op12 && op21 == op22 || op11 == op22 && op21 == op12
    Sub _ op11 op21             == Sub _ op12 op22             = op11 == op12 && op21 == op22
    Mul _ op11 op21             == Mul _ op12 op22             = op11 == op12 && op21 == op22 || op11 == op22 && op21 == op12
    Sdiv _ op11 op21            == Sdiv _ op12 op22            = op11 == op12 && op21 == op22
    Srem _ op11 op21            == Srem _ op12 op22            = op11 == op12 && op21 == op22
    Xor _ op11 op21             == Xor _ op12 op22             = op11 == op12 && op21 == op22 || op11 == op22 && op21 == op12
    Icmp _ Eq op11 op21         == Icmp _ Eq op12 op22         = op11 == op12 && op21 == op22 || op11 == op22 && op21 == op12
    Icmp _ Ne op11 op21         == Icmp _ Ne op12 op22         = op11 == op12 && op21 == op22 || op11 == op22 && op21 == op12
    Icmp _ Sgt op11 op21        == Icmp _ contidion2 op12 op22 = contidion2 == Sgt && op11 == op12 && op21 == op22 || contidion2 == Slt && op11 == op22 && op21 == op12
    Icmp _ Sge op11 op21        == Icmp _ contidion2 op12 op22 = contidion2 == Sge && op11 == op12 && op21 == op22 || contidion2 == Sle && op11 == op22 && op21 == op12
    Icmp _ Slt op11 op21        == Icmp _ contidion2 op12 op22 = contidion2 == Slt && op11 == op12 && op21 == op22 || contidion2 == Sgt && op11 == op22 && op21 == op12
    Icmp _ Sle op11 op21        == Icmp _ contidion2 op12 op22 = contidion2 == Sle && op11 == op12 && op21 == op22 || contidion2 == Sge && op11 == op22 && op21 == op12
    Icmp _ contidion1 op11 op21 == Icmp _ contidion2 op12 op22 = contidion1 == contidion2 && op11 == op12 && op21 == op22
    Call _ _ name1 args1        == Call _ _ name2 args2        = name1 == name2 && name1 == "@_concatStrings" && args1 == args2
    Phi _ values1               == Phi _ values2               = values1 == values2
    Bitcast _ n1 pointer1       == Bitcast _ n2 pointer2       = n1 == n2 && pointer1 == pointer2
    _ == _ = False

equalInstructions :: Instruction -> [Instruction]
equalInstructions (Add _ op1 op2)             = [Add "_" op1 op2, Add "_" op2 op1]
equalInstructions (Sub _ op1 op2)             = [Sub "_" op1 op2]
equalInstructions (Mul _ op1 op2)             = [Mul "_" op1 op2, Mul "_" op2 op1]
equalInstructions (Sdiv _ op1 op2)            = [Sdiv "_" op1 op2]
equalInstructions (Srem _ op1 op2)            = [Srem "_" op1 op2]
equalInstructions (Xor _ op1 op2)             = [Xor "_" op1 op2, Xor "_" op2 op1]
equalInstructions (Icmp _ Eq op1 op2)         = [Icmp "_" Eq op1 op2, Icmp "_" Eq op2 op1]
equalInstructions (Icmp _ Ne op1 op2)         = [Icmp "_" Ne op1 op2, Icmp "_" Eq op2 op1]
equalInstructions (Icmp _ Sgt op1 op2)        = [Icmp "_" Sgt op1 op2, Icmp "_" Slt op2 op1]
equalInstructions (Icmp _ Sge op1 op2)        = [Icmp "_" Sge op1 op2, Icmp "_" Sle op2 op1]
equalInstructions (Icmp _ Slt op1 op2)        = [Icmp "_" Slt op1 op2, Icmp "_" Sgt op2 op1]
equalInstructions (Icmp _ Sle op1 op2)        = [Icmp "_" Sle op1 op2, Icmp "_" Sge op2 op1]
equalInstructions (Call _ ty name args)       | name == "@_concatStrings" = [Call "_" ty name args]
equalInstructions (Phi _ values)              = [Phi "_" values]
equalInstructions (Bitcast _ n pointer)       = [Bitcast "_" n pointer]
equalInstructions i = [i]


data BasicBlock = BasicBlock
    { label        :: Label
    , predecessors :: [Label]
    , instructions :: [Instruction]
    , terminator   :: Instruction
    }

instance Show BasicBlock where
    show (BasicBlock label _ instructions terminator) =
        showLabel . showInstructions . showTerminator $ "\n"
        where
            showLabel = shows (Label label) . showChar '\n'
            showInstructions = foldr (\instruction -> (.) (indent . shows instruction . showChar '\n')) id instructions
            showTerminator = indent . shows terminator
            indent = showString "    "


data Function = Function
    { name        :: Name
    , returnTy    :: Ty
    , args        :: [Value]
    , basicBlocks :: [BasicBlock]
    }

instance Show Function where
    show (Function name returnTy args basicBlocks) =
        showHeader . showBlocks $ "}\n\n"
        where
            showHeader = showString ("define " ++ show returnTy ++ " " ++ name ++ "(") . showArgs . showString ") {\n"
            showArgs = showString . intercalate ", " $ map showTyAndValue args
            showBlocks = foldr ((.) . shows) id basicBlocks


typeToTy :: Type a -> Ty
typeToTy (Int _)  = I32
typeToTy (Str _)  = Ptr I8
typeToTy (Bool _) = I1
typeToTy (Void _) = VoidTy

tyOfValue :: Value -> Ty
tyOfValue NoValue              = VoidTy
tyOfValue (RegisterValue ty _) = ty
tyOfValue (IntegerValue _)     = I32
tyOfValue (BoolValue _)        = I1

showTyAndValue :: Value -> String
showTyAndValue value = (show $ tyOfValue value) ++ " " ++ (show value)
