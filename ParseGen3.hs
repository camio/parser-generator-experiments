module ParseGen3 where

import qualified Data.Vector as V

type Symbol = Int

newtype Rule = Rule (V.Vector Symbol)
  deriving (Show, Eq)

-- TODO Grammar's constructor should be private since it is illegal to have an
-- empty element in this array.
newtype Grammar = Grammar (V.Vector [Rule])
  deriving (Show, Eq)

isTerminal :: Grammar -> Symbol -> Bool
isTerminal (Grammar v) = (>= V.length v)

data ParseTree
  = PTRule Symbol Int [ParseTree]
  | PTTerm Symbol
  deriving (Show, Eq)

-- Return a list of rules in the specified grammar corresponding to the
-- specified variable.
findRules :: Grammar -> Symbol -> [Rule]
findRules (Grammar g) s = g V.! s

-- Return a list of possible parse trees for the specified symbol using the
-- specified grammar.
genParses :: Grammar -> Symbol -> [ParseTree]
genParses g s | isTerminal g s = [PTTerm s]
genParses g s =
  concat $
    zipWith (genRuleParses g s) [0 ..] $
      findRules g s

genRuleParses :: Grammar -> Symbol -> Int -> Rule -> [ParseTree]
genRuleParses g s idx (Rule v) = map (PTRule s idx) $ genRuleParses' v
  where
    genRuleParses' :: V.Vector Symbol -> [[ParseTree]]
    genRuleParses' v | V.null v = [[]]
    genRuleParses' v =
      let Just (e, es) = V.uncons v -- TODO Could use unsafe constructs here for efficiency
       in [ eParse : esParse
            | eParse <- genParses g e,
              esParse <- genRuleParses' es
          ]
