module Main (main) where

import qualified MyLib (someFunc)
import qualified Algo (amplify)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
