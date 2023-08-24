module ParseGen4 where

import qualified Data.Vector as V
import Debug.Trace

type Symbol = Int

-- A datatype representing the right hand side of a rule.
newtype RuleRhs = RuleRhs (V.Vector Symbol)
  deriving (Show, Eq)

-- TODO IntGrammar's constructor should be private since it is illegal to have an
-- empty element in this array.
newtype IntGrammar = IntGrammar (V.Vector [RuleRhs])
  deriving (Show, Eq)

-- Return 'True' if, in the given grammar, the given symbol is a terminal.
isTerminal :: IntGrammar -> Symbol -> Bool
isTerminal (IntGrammar v) = (>= V.length v)

data ParseTree
  = PTRule Symbol Int [ParseTree]
  | PTTerm Symbol
  deriving (Show, Eq)

-- Return rules in the given grammar with the given nonterminal on its
-- left-hand side.
findRules :: IntGrammar -> Symbol -> [RuleRhs]
findRules (IntGrammar g) s = g V.! s

-- Return a list of possible parse trees for the given symbol using the
-- given grammar. Optionally specify 'maxDepth' to limit the depth of the
-- generated parse trees.
genParses :: Maybe Int -> IntGrammar -> Symbol -> [ParseTree]
genParses maxDepth g s | isTerminal g s = [PTTerm s]
genParses (Just maxDepth) _ _ | maxDepth < 1 = []
genParses maxDepth g s =
  concat $
    zipWith (genRuleParses (fmap (\a -> a - 1) maxDepth) g s) [0 ..] $
      findRules g s

-- Return a list of possible parse trees for the given rule with the given
-- grammar. The rule is specified with its left hand side symbol, its index, and
-- its right hand side. Optionally specify 'maxDepth' to limit the depth of the
-- generated parse trees.
genRuleParses :: Maybe Int -> IntGrammar -> Symbol -> Int -> RuleRhs -> [ParseTree]
genRuleParses maxDepth g lhs idx (RuleRhs v) = map (PTRule lhs idx) $ genRuleParses' v
  where
    genRuleParses' :: V.Vector Symbol -> [[ParseTree]]
    genRuleParses' v | V.null v = [[]]
    genRuleParses' v =
      let Just (e, es) = V.uncons v -- TODO Use unsafe construct here for efficiency?
       in [ eParse : esParse
            | eParse <- genParses maxDepth g e,
              esParse <- genRuleParses' es
          ]
