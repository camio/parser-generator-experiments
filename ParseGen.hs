module ParseGen where

data Symbol = S1 | S2 | S3 | S4 | S5
  deriving (Show, Eq)

data Terminal = T1 | T2 | T3 | T4
  deriving (Show, Eq)

data Rule = Rule {lhs :: Symbol, rhs :: [Either Symbol Terminal]}
  deriving (Show, Eq)

newtype Grammar = Grammar [Rule]
  deriving (Show, Eq)

findRules :: Grammar -> Symbol -> [Rule]
findRules (Grammar g) s = filter ((==) s . lhs) g

genSymTermParses :: Grammar -> Either Symbol Terminal -> [[Terminal]]
genSymTermParses g = either (genSymParses g) (pure . pure)

genSymTermsParses :: Grammar -> [Either Symbol Terminal] -> [[Terminal]]
genSymTermsParses g [] = [[]]
genSymTermsParses g (e : es) =
  [ eParse ++ esParse
    | eParse <- genSymTermParses g e,
      esParse <- genSymTermsParses g es
  ]

genSymParses :: Grammar -> Symbol -> [[Terminal]]
genSymParses g = concatMap (genSymTermsParses g . rhs) . findRules g
