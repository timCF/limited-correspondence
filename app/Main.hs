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
      putStrLn $ "Case " ++ show caseNumber ++ ": " ++ chooseSolution (permutate pairs "" "" [])
      mainLoop $ caseNumber + 1

getPairs :: Integer -> [((String, String), Integer)] -> IO [((String, String), Integer)]
getPairs 0 acc          = return acc
getPairs countdown acc  = do
  rawPair <- getLine
  let
    [first, second] = words rawPair
    pair            = ((first, second), countdown)
    in getPairs (countdown - 1) $ pair:acc

permutate :: [((String, String), Integer)] -> String -> String -> [String] -> [String]
permutate []    str1 str2 acc
  | str1 == str2  = str1:acc
  | otherwise     = acc
permutate pairs str1 str2 acc =
  if str1 == str2 && str1 /= ""
    then str1:acc
    else
      foldl (generateFolder pairs str1 str2) acc pairs

generateFolder :: [((String, String), Integer)] -> String -> String -> ([String] -> ((String, String), Integer) -> [String])
generateFolder pairs str1 str2 = folder
  where
    folder acc ((sub1, sub2), index) =
      let
        newPairs = filter (\ ((_, _), xindex) -> xindex /= index) pairs
        in permutate newPairs (str1 ++ sub1) (str2 ++ sub2) acc

chooseSolution :: [String] -> String
chooseSolution []         = "IMPOSSIBLE"
chooseSolution [solution] = solution
chooseSolution (x:xs)     =
  let
    (newLst, _) = foldl minimumByStrLen ([x], length x) xs
    in minimum newLst
  where
    minimumByStrLen (acc, len) newStr =
      let
        newLen = length newStr
      in
        case compare newLen len of
          LT -> ([newStr], newLen)
          GT -> (acc, len)
          EQ -> (newStr:acc, len)
