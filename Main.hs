
module Main where
import TF

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
  let results = find24 [read x1, read x2, read x3, read x4]
  mapM_ putStrLn results
