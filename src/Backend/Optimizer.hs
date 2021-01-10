module Backend.Optimizer where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.List (partition)
import Backend.LLVMAsm


removeUnreachableBlocks :: Function -> Function
removeUnreachableBlocks (Function name returnTy args basicBlocks) =
    Function name returnTy args $ filter reachable basicBlocks
    where
        succs :: Map.Map Label [Label]
        succs = foldr succsAcc Map.empty basicBlocks
        succsAcc :: BasicBlock -> Map.Map Label [Label] -> Map.Map Label [Label]
        succsAcc (BasicBlock pred _ _ (BrConditional _ labelTrue labelFalse)) map = insertEdge pred labelTrue $ insertEdge pred labelFalse map
        succsAcc (BasicBlock pred _ _ (BrUnconditional label)) map                = insertEdge pred label map
        succsAcc _ map                                                            = map
        successors :: Label -> [Label]
        successors label = fromMaybe [] $ Map.lookup label succs
        reachableFromEntry :: Set.Set Label
        reachableFromEntry = reachableVertices "%entry" successors
        reachable :: BasicBlock -> Bool
        reachable block = Set.member (label block) reachableFromEntry
        reachableVertices :: Ord a => a -> (a -> [a]) -> Set.Set a
        reachableVertices initial neighbours =
            visit initial Set.empty
            where
                visit v visited =
                    foldr (\u acc -> if Set.member u acc then acc else visit u acc) (Set.insert v visited) (neighbours v)

insertPredecessors :: Function -> Function
insertPredecessors (Function name returnTy args basicBlocks) =
    Function name returnTy args blocksWithPreds
    where
        preds :: Map.Map Label [Label]
        preds = foldr predsAcc Map.empty basicBlocks
        predsAcc :: BasicBlock -> Map.Map Label [Label] -> Map.Map Label [Label]
        predsAcc (BasicBlock pred _ _ (BrConditional _ labelTrue labelFalse)) map = insertEdge labelTrue pred $ insertEdge labelFalse pred map
        predsAcc (BasicBlock pred _ _ (BrUnconditional label)) map                = insertEdge label pred map
        predsAcc _ map                                                            = map
        blocksWithPreds :: [BasicBlock]
        blocksWithPreds = map (\block -> block { predecessors = fromMaybe [] $ Map.lookup (label block) preds }) basicBlocks

insertEdge :: Label -> Label -> Map.Map Label [Label] -> Map.Map Label [Label]
insertEdge key value = Map.insertWith (++) key [value]

moveAllocasToEntry :: Function -> Function
moveAllocasToEntry (Function name returnTy args basicBlocks) =
    Function name returnTy args basicBlocks'
    where
        basicBlocks' :: [BasicBlock]
        basicBlocks' = fst $ foldr blocksAllocasAcc ([], []) basicBlocks
        blocksAllocasAcc :: BasicBlock -> ([BasicBlock], [Instruction]) -> ([BasicBlock], [Instruction])
        blocksAllocasAcc block (blocks, allocas) =
            if label block == "%entry"
            then
                (block { instructions = allocas ++ instructions block } : blocks, [])
            else
                let
                    blocksAllocas, blocksRemainder :: [Instruction]
                    (blocksAllocas, blocksRemainder) = partition isAlloca $ instructions block
                    isAlloca :: Instruction -> Bool
                    isAlloca (Alloca _ _) = True
                    isAlloca _            = False
                in
                (block { instructions = blocksRemainder } : blocks, blocksAllocas ++ allocas)


optimizeFunction :: Function -> Function
optimizeFunction = moveAllocasToEntry . removeUnreachableBlocks
