
module Main where
import TF

main :: IO ()
main = do
  mapM_ (\(x, ys) -> do
    if length ys == 0 then
        return ()
    else print x) all24
