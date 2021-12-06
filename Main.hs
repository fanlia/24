
module Main where

fs :: [(String, Double->Double->Double)]
fs = [(" + ", (+)), (" - ", (-)), (" * ", (*)), (" / ", (/))]

find :: [Double] -> [String]
find xs = [p | x1 <- xs, x2 <- xs, x3 <- xs, x4 <- xs, x1 /= x2, x1 /= x3, x1 /= x4, x2 /= x3, x2 /= x4, x3 /= x4,
  (n1, f1) <- fs, (n2, f2) <- fs, (n3, f3) <- fs,
  (p, v) <- [
    ("((" ++ show x1 ++ n1 ++ show x2 ++ ")" ++ n2 ++ show x3 ++ ")" ++ n3 ++ show x4, ((x1 `f1` x2) `f2` x3) `f3` x4),
    ("(" ++ show x1 ++ n1 ++ show x2 ++ ")" ++ n2 ++ "(" ++ show x3 ++ n3 ++ show x4 ++ ")", (x1 `f1` x2) `f2` (x3 `f3` x4)),
    (show x1 ++ n1 ++ "((" ++ show x2 ++ n2 ++ show x3 ++ ")" ++ n3 ++ show x4 ++ ")", x1 `f1` ((x2 `f2` x3) `f3` x4)),
    ("(" ++ show x1 ++ n1 ++ "("++ show x2 ++ n2 ++ show x3 ++ "))" ++ n3 ++ show x4, (x1 `f1` (x2 `f2` x3)) `f3` x4),
    (show x1 ++ n1 ++ "(" ++ show x2 ++ n2 ++ "(" ++ show x3 ++ n3 ++ show x4 ++ "))", x1 `f1` (x2 `f2` (x3 `f3` x4)))
  ],
  v == 24
  ]

main :: IO ()
main = do
  putStr "x1="
  x1 <- getLine
  putStr "x2="
  x2 <- getLine
  putStr "x3="
  x3 <- getLine
  putStr "x4="
  x4 <- getLine
  let results = find [read x1, read x2, read x3, read x4]
  mapM_ putStrLn results
