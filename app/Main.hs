module Main where

import System.IO (isEOF)

main :: IO ()
main = mainLoop 1

mainLoop :: Integer -> IO ()
mainLoop caseNumber = do
  eof <- isEOF
  if eof
    then return ()
    else do
      qty          <- getLine
      (lst1, lst2) <- getLists (read qty) [] []
      putStrLn $ "Case " ++ show caseNumber ++ ": " ++ show (lst1, lst2)
      mainLoop $ caseNumber + 1

getLists :: Integer -> [String] -> [String] -> IO ([String], [String])
getLists 0         xs1 xs2 = return (xs1, xs2)
getLists countdown xs1 xs2 = do
  rawPair <- getLine
  let
    [x1, x2] = words rawPair
    in getLists (countdown - 1) (x1:xs1) (x2:xs2)
