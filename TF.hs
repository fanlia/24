
module TF (
    showr,
    find24,
    all24,
) where

showr :: Double -> String
showr r = show (round r)

fs :: [(String, Double->Double->Double)]
fs = [(" + ", (+)), (" - ", (-)), (" * ", (*)), (" / ", (/))]

find24 :: [Double] -> [String]
find24 xs = [p | (i1, x1) <- ixs, (i2, x2) <- ixs, (i3, x3) <- ixs, (i4, x4) <- ixs, i1 /= i2, i1 /= i3, i1 /= i4, i2 /= i3, i2 /= i4, i3 /= i4,
  (n1, f1) <- fs, (n2, f2) <- fs, (n3, f3) <- fs,
  (p, v) <- [
    ("((" ++ showr x1 ++ n1 ++ showr x2 ++ ")" ++ n2 ++ showr x3 ++ ")" ++ n3 ++ showr x4, ((x1 `f1` x2) `f2` x3) `f3` x4),
    ("(" ++ showr x1 ++ n1 ++ showr x2 ++ ")" ++ n2 ++ "(" ++ showr x3 ++ n3 ++ showr x4 ++ ")", (x1 `f1` x2) `f2` (x3 `f3` x4)),
    (showr x1 ++ n1 ++ "((" ++ showr x2 ++ n2 ++ showr x3 ++ ")" ++ n3 ++ showr x4 ++ ")", x1 `f1` ((x2 `f2` x3) `f3` x4)),
    ("(" ++ showr x1 ++ n1 ++ "("++ showr x2 ++ n2 ++ showr x3 ++ "))" ++ n3 ++ showr x4, (x1 `f1` (x2 `f2` x3)) `f3` x4),
    (showr x1 ++ n1 ++ "(" ++ showr x2 ++ n2 ++ "(" ++ showr x3 ++ n3 ++ showr x4 ++ "))", x1 `f1` (x2 `f2` (x3 `f3` x4)))
  ],
  v == 24
  ]
  where ixs = zip [0..] xs

all24 :: [([Int], [String])]
all24 = let n = 10 in [([round x1, round x2, round x3, round x4], find24 [x1, x2, x3, x4]) | x1 <- [1..n], x2 <- [x1..n], x3 <- [x2..n], x4 <- [x3..n]]
