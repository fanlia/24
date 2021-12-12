
module TF (
    find24,
    showr,
) where

showr :: Double -> String
showr r = show (round r)

fs :: [(String, Double->Double->Double)]
fs = [(" + ", (+)), (" - ", (-)), (" * ", (*)), (" / ", (/))]

find24 :: [Double] -> [String]
find24 xs = [p | x1 <- xs, x2 <- xs, x3 <- xs, x4 <- xs, x1 /= x2, x1 /= x3, x1 /= x4, x2 /= x3, x2 /= x4, x3 /= x4,
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

