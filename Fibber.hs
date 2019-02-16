import Criterion.Main

-- |recursive fibonacci function
fibr m | m < 0     = error "negative"
       | otherwise = go m
    where
        go 0 = 0
        go 1 = 1
        go n = go (n-1) + go (n-2)

-- |iterative fibonacci function
fibi a | a < 0     = error "negative"
       | otherwise = go a
    where 
        go b = take b . map head $ iterate (\(x:y:xs) -> (x+y):x:xs) [0,1]

-- |using the Criterion module, a Haskell libra
-- |ry for measuring and analysing software per
-- |formance I wrote two driver functions which
-- |which built and ran benchmarks.
main = defaultMain [
    bgroup "fibr" [ bench "1"   $ whnf fibr 1
                  , bench "5"   $ whnf fibr 5 
                  , bench "11"  $ whnf fibr 11
                  , bench "34"  $ whnf fibr 34
                  ],
    bgroup "fibi" [ bench "1"   $ whnf fibi 1
                  , bench "5"   $ whnf fibr 5 
                  , bench "11"  $ whnf fibr 11
                  , bench "34"  $ whnf fibr 34
                  ]
                  ]

