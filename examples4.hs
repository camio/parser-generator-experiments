import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Data.Maybe
import Debug.Trace

import ParseGen4
  ( IntGrammar (..),
    ParseTree (..),
    RuleRhs (..),
    Symbol,
    genParses,
  )

data StringGrammar = StringGrammar
  { mapping :: V.Vector String,
    intGrammar :: IntGrammar
  }

instance Show StringGrammar where
  show g = T.unpack $ prettyPrintG2 (mapping g V.!) 40 (intGrammar g)

prettyPrintG2 :: (Int-> String) -> Int -> IntGrammar -> T.Text
prettyPrintG2 showI justification (IntGrammar v) = T.unlines $ zipWith prettyPrintRules [0 ..] (V.toList v)
  where
    prettyPrintRules :: Symbol -> [RuleRhs] -> T.Text
    prettyPrintRules s rules = T.unlines $ zipWith (prettyPrintRule s) [0 ..] rules
    prettyPrintRule :: Symbol -> Int -> RuleRhs -> T.Text
    prettyPrintRule s i r = T.justifyLeft justification ' ' (T.pack $ showI s ++ " := " ++ prettyPrintRuleRhs r) `T.append` T.pack ("# " ++ showI s ++ "_" ++ show i)
    prettyPrintRuleRhs :: RuleRhs -> String
    prettyPrintRuleRhs (RuleRhs v) = unwords $ map showI $ V.toList v

buildStringGrammar :: [[String]] -> StringGrammar
buildStringGrammar rules =
  let nonterminals :: [String]
      nonterminals = map head $ group $ sort $ map head rules
      ntmap :: M.Map String Int
      ntmap = M.fromList $ zip nonterminals [0 ..]
      terminals :: [String]
      terminals = filter (`M.notMember` ntmap) $ map head $ group $ sort $ concat rules
      mapping :: V.Vector String
      mapping = V.fromList (nonterminals ++ terminals)
      tmap :: M.Map String Int
      tmap = M.fromList $ zip terminals [M.size ntmap ..]
      smap :: M.Map String Int
      smap = M.union ntmap tmap
      intRules :: [[Int]]
      intRules = sortBy (\a b -> compare (head a) (head b)) $ (map.map) (fromJust . (`M.lookup` smap)) rules
      intGrammar :: IntGrammar
      intGrammar = buildGrammar intRules
   in StringGrammar mapping intGrammar

conditional =
  buildStringGrammar
    [ ["E", "Lit"],
      ["E", "Cond"],
      ["Cond", "if", "E", "then", "E", "else", "E"],
      ["Cond", "if", "E", "then", "E"],
      ["Lit", "0"],
      ["Lit", "1"],
      ["Lit", "2"],
      ["Lit", "3"]
    ]

-- Input Format:
-- - '[3, 5, 6, 7]' corresponds to '3 := 5 6 7'
-- - Every non-terminal must have at least one rule.
-- - Rules for non-terminals must be in sorted order.
buildGrammar :: [[Int]] -> IntGrammar
buildGrammar rules = IntGrammar ruleGroups''
  where
    toRuleRhs :: [Int] -> RuleRhs
    toRuleRhs = RuleRhs . V.fromList . tail

    ruleGroups :: [[[Int]]]
    ruleGroups = groupBy (\a b -> head a == head b) rules

    ruleGroups' :: [[RuleRhs]]
    ruleGroups' = (map . map) toRuleRhs ruleGroups

    ruleGroups'' :: V.Vector [RuleRhs]
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

prettyPrintG :: IntGrammar -> T.Text
prettyPrintG (IntGrammar v) = T.unlines $ zipWith prettyPrintRules [0 ..] (V.toList v)
  where
    prettyPrintRules :: Symbol -> [RuleRhs] -> T.Text
    prettyPrintRules s rules = T.unlines $ zipWith (prettyPrintRule s) [0 ..] rules
    prettyPrintRule :: Symbol -> Int -> RuleRhs -> T.Text
    prettyPrintRule s i r = T.justifyLeft 20 ' ' (T.pack $ show s ++ " := " ++ prettyPrintRuleRhs r) `T.append` T.pack ("# " ++ show s ++ "_" ++ show i)
    prettyPrintRuleRhs :: RuleRhs -> String
    prettyPrintRuleRhs (RuleRhs v) = unwords $ map show $ V.toList v

prettyPrintP :: ParseTree -> String
prettyPrintP (PTRule s i pts) = concatMap prettyPrintP pts
prettyPrintP (PTTerm t) = show t

prettyPrintPT :: ParseTree -> String
prettyPrintPT (PTRule s i pts) = "(" ++ show s ++ "_" ++ show i ++ concatMap ((" " ++) . prettyPrintPT) pts ++ ")"
prettyPrintPT (PTTerm t) = show t

symbolToString = (V.!) . mapping

stringToSymbol g s = V.findIndex (== s) (mapping g)

prettyPrintPG :: StringGrammar -> ParseTree -> String
prettyPrintPG g (PTRule s i pts) = unwords $ map (prettyPrintPG g) pts
prettyPrintPG g (PTTerm t) = symbolToString g t

prettyPrintPTG :: StringGrammar -> ParseTree -> String
prettyPrintPTG g (PTRule lhs i pts) = "(" ++ symbolToString g lhs ++ "_" ++ show i ++ concatMap ((" " ++) . prettyPrintPTG g) pts ++ ")"
prettyPrintPTG g (PTTerm t) = symbolToString g t

t = flip (genParses Nothing) 0

t2 g = do
  TIO.putStrLn $ prettyPrintG g
  TIO.putStrLn $ T.unlines $ map (\p -> T.justifyLeft 10 ' ' (T.pack (prettyPrintP p)) `T.append` T.pack (" <- " ++ prettyPrintPT p)) $ genParses Nothing g 0

t3 g nonterminal depth = do
  print g
  let Just nonterminalSymbol = stringToSymbol g nonterminal
  let parses = genParses (Just depth) (intGrammar g) nonterminalSymbol
  TIO.putStrLn
    $ T.unlines
    $ map (\p -> T.justifyLeft 10 ' ' (T.pack (prettyPrintPG g p)) `T.append` T.pack (" <- " ++ prettyPrintPTG g p))
    $ parses
