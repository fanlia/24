
module Main where
import TF

main :: IO ()
main = do
  mapM_ (\(x, ys) -> do
    if length ys == 0
        then print x
        else return ()) all24
