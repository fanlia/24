
module Main where
import TF

main :: IO ()
main = do
  mapM_ (\(x, ys) -> do
    print x
    mapM_ putStrLn ys
    putStrLn "") all24
