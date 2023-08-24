import ParseGen

simple =
  Grammar
    [ Rule S1 [Right T1]
    ]

twoOrFour =
  Grammar
    [ Rule S1 [Right T1, Right T1, Right T1, Right T1],
      Rule S1 [Right T1, Right T1]
    ]

twoOrFourAgain =
  Grammar
    [ Rule S1 [Right T1, Right T1, Left S2],
      Rule S2 [Right T1, Right T1],
      Rule S2 []
    ]

branchy =
  Grammar
    [ Rule S1 [Left S2, Left S3]
    , Rule S2 [Right T2, Right T2]
    , Rule S2 []
    , Rule S3 [Right T3, Right T3]
    , Rule S3 [Right T3]
    , Rule S3 []
    ]

t = (flip genSymParses) S1
