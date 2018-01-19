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
      qty   <- getLine
      pairs <- getPairs (read qty) []
      putStrLn $ "Case " ++ show caseNumber ++ ": " ++ permutate pairs "" "" "IMPOSSIBLE"
      mainLoop $ caseNumber + 1

getPairs :: Integer -> [((String, String), Integer)] -> IO [((String, String), Integer)]
getPairs 0 acc          = return acc
getPairs countdown acc  = do
  rawPair <- getLine
  let
    [first, second] = words rawPair
    pair            = ((first, second), countdown)
    in getPairs (countdown - 1) $ pair:acc

permutate :: [((String, String), Integer)] -> String -> String -> String -> String
permutate []    str1 str2 acc
  | str1 == str2  = chooseSolution str1 acc
  | otherwise     = acc
permutate pairs str1 str2 acc =
  if str1 == str2 && str1 /= ""
    then chooseSolution str1 acc
    else
      let
        str1len  = length str1
        str2len  = length str2
        continue =
          if str1len < str2len
            then str1 == take str1len str2
            else str2 == take str2len str1
      in
        if continue
          then foldl folder acc pairs
          else acc
  where
    folder acc ((sub1, sub2), index) =
      let
        newPairs = filter (\ ((_, _), xindex) -> xindex /= index) pairs
        in permutate newPairs (str1 ++ sub1) (str2 ++ sub2) acc

chooseSolution :: String -> String -> String
chooseSolution solution     "IMPOSSIBLE" = solution
chooseSolution newSolution  oldSolution  =
  case compare (length newSolution) (length oldSolution) of
    LT -> newSolution
    GT -> oldSolution
    EQ -> min newSolution oldSolution
