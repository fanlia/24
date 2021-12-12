
module Main where
import TF

no24 :: [String]
no24 = let n = 10 in [ showr x1 ++ ", " ++ showr x2 ++ ", " ++ showr x3 ++ ", " ++ showr x4 | x1 <- [1..n], x2 <- [x1..n], x3 <- [x2..n], x4 <- [x3..n], let v = find24 [x1, x2, x3, x4], length v == 0]

main :: IO ()
main = do
  mapM_ putStrLn no24
