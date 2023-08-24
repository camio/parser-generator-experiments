import ParseGen2

simple =
  Grammar
    [ Rule V1 [Right T1]
    ]

twoOrFour =
  Grammar
    [ Rule V1 [Right T1, Right T1, Right T1, Right T1],
      Rule V1 [Right T1, Right T1]
    ]

twoOrFourAgain =
  Grammar
    [ Rule V1 [Right T1, Right T1, Left V2],
      Rule V2 [Right T1, Right T1],
      Rule V2 []
    ]

branchy =
  Grammar
    [ Rule V1 [Left V2, Left V3]
    , Rule V2 [Right T2, Right T2]
    , Rule V2 []
    , Rule V3 [Right T3, Right T3]
    , Rule V3 [Right T3]
    , Rule V3 []
    ]
prettyPrintPT :: ParseTree -> String
prettyPrintPT (PTRule rule pts) = "(" ++ show (lhs rule) ++ concatMap ((" " ++) . prettyPrintPT) pts ++ ")"
prettyPrintPT (PTTerm t) = show t

t = (flip genVarParses) V1
t2 = putStrLn . unlines . map prettyPrintPT . (flip genVarParses) V1
