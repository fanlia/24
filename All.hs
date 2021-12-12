
module Main where
import TF

all24 :: [String]
all24 = let n = 10 in [v | x1 <- [1..n], x2 <- [x1..n], x3 <- [x2..n], x4 <- [x3..n], v <- find24 [x1, x2, x3, x4]]

main :: IO ()
main = do
  mapM_ putStrLn all24
