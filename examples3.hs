import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import ParseGen3

-- TODO: This, https://forums.swift.org/t/goals-for-forward-interop/59100

-- Input Format:
-- - '[3, 5, 6, 7]' corresponds to '3 := 5 6 7'
-- - Every non-terminal must have at least one rule.
-- - Rules for non-terminals must be in sorted order.
buildGrammar :: [[Int]] -> Grammar
buildGrammar rules = Grammar ruleGroups''
  where
    toRule :: [Int] -> Rule
    toRule = Rule . V.fromList . tail

    ruleGroups :: [[[Int]]]
    ruleGroups = groupBy (\a b -> head a == head b) rules

    ruleGroups' :: [[Rule]]
    ruleGroups' = (map . map) toRule ruleGroups

    ruleGroups'' :: V.Vector [Rule]
    ruleGroups'' = V.fromList ruleGroups'

simple = buildGrammar [[0, 1]]

twoOrFour =
  buildGrammar
    [ [0, 1, 1, 1, 1],
      [0, 1, 1]
    ]

twoOrFourAgain =
  buildGrammar
    [ [0, 2, 2, 1],
      [1, 2, 2],
      [1]
    ]

branchy =
  buildGrammar
    [ [0, 1, 2],
      [1, 3, 3],
      [1],
      [2, 4, 4],
      [2, 4],
      [2]
    ]

-- S0 := if S1 then S1 else S1
-- S0 := if S1 then S1
-- S1 := e1
-- S1 := e2
-- S1 := e3
--
-- S0 -> 0
-- S1 -> 1
-- if -> 2
-- then -> 3
-- else -> 4
-- e1 -> 5
-- e2-> 6
-- e3-> 7
ifThenElse =
  buildGrammar
    [ [0, 2, 1, 3, 1, 4, 1],
      [0, 2, 1, 3, 1],
      [1, 5],
      [1, 6],
      [1, 7]
    ]

prettyPrintG :: Grammar -> T.Text
prettyPrintG (Grammar v) = T.unlines $ zipWith prettyPrintRules [0 ..] (V.toList v)
  where
    prettyPrintRules :: Symbol -> [Rule] -> T.Text
    prettyPrintRules s rules = T.unlines $ zipWith (prettyPrintRule s) [0 ..] rules
    prettyPrintRule :: Symbol -> Int -> Rule -> T.Text
    prettyPrintRule s i r = T.justifyLeft 20 ' ' (T.pack $ show s ++ " := " ++ prettyPrintRuleRhs r) `T.append` T.pack ("# " ++ show s ++ "_" ++ show i)
    prettyPrintRuleRhs :: Rule -> String
    prettyPrintRuleRhs (Rule v) = unwords $ map show $ V.toList v

prettyPrintP :: ParseTree -> String
prettyPrintP (PTRule s i pts) = concatMap prettyPrintP pts
prettyPrintP (PTTerm t) = show t

prettyPrintPT :: ParseTree -> String
prettyPrintPT (PTRule s i pts) = "(" ++ show s ++ "_" ++ show i ++ concatMap ((" " ++) . prettyPrintPT) pts ++ ")"
prettyPrintPT (PTTerm t) = show t

t = flip genParses 0

t2 g = do
  TIO.putStrLn $ prettyPrintG g
  TIO.putStrLn $ T.unlines $ map (\p -> T.justifyLeft 10 ' ' (T.pack (prettyPrintP p)) `T.append` T.pack (" <- " ++ prettyPrintPT p)) $ genParses g 0
