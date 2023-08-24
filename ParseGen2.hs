module ParseGen2 where

data Variable = V1 | V2 | V3 | V4 | V5
  deriving (Show, Eq)

data Terminal = T1 | T2 | T3 | T4
  deriving (Show, Eq)

data Rule = Rule {lhs :: Variable, rhs :: [Either Variable Terminal]}
  deriving (Show, Eq)

data ParseTree
  = PTRule Rule [ParseTree]
  | PTTerm Terminal
  deriving (Show, Eq)

newtype Grammar = Grammar [Rule]
  deriving (Show, Eq)

-- Return a list of rules in the specified grammar corresponding to the
-- specified variable.
findRules :: Grammar -> Variable -> [Rule]
findRules (Grammar g) s = filter ((==) s . lhs) g

-- Return a list of possible parse trees for the specified variable or terminal
-- using the specified grammar.
genVarTermParses :: Grammar -> Either Variable Terminal -> [ParseTree]
genVarTermParses g = either (genVarParses g) (pure . PTTerm)

-- Return a list of possible parse tree lists for the specified variable/terminal
-- list using the specified grammar. The variable/terminal list is interpreted as
-- the right hand side of a production rule.
genVarTermsParses :: Grammar -> [Either Variable Terminal] -> [[ParseTree]]
genVarTermsParses g [] = [[]]
genVarTermsParses g (e : es) =
  [ eParse : esParse
    | eParse <- genVarTermParses g e,
      esParse <- genVarTermsParses g es
  ]

-- Return a list of possible parse tree lists for the specified rule
-- using the specified grammar
genRuleParses :: Grammar -> Rule -> [ParseTree]
genRuleParses g rule = map (PTRule rule) $ genVarTermsParses g (rhs rule)

-- Return a list of possible parse tree lists for the specified variable
-- using the specified grammar
genVarParses :: Grammar -> Variable -> [ParseTree]
genVarParses g = concatMap (genRuleParses g) . findRules g
