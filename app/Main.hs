module Main where

import System.IO
import Lib

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  s <- getLine
  putStrLn $ testLisp s
  main
