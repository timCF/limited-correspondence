module Main where

import System.IO (isEOF)

main :: IO ()
main = loop 1

loop :: Integer -> IO ()
loop caseNumber = do
  eof <- isEOF
  if eof
    then return ()
    else do
      qty   <- getLine
      pairs <- getPairs (read qty) []
      putStrLn $ "Case " ++ show caseNumber ++ ": " ++ show pairs
      loop $ caseNumber + 1

getPairs :: Integer -> [(String, String)] -> IO [(String, String)]
getPairs 0 acc          = return acc
getPairs countdown acc  = do
  rawPair <- getLine
  let
    [first, second] = words rawPair
    pair            = (first, second)
    in getPairs (countdown - 1) $ pair:acc
