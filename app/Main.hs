module Main where

import System.IO (isEOF)

-- data-related stuff

data Buffer = Buffer {
  value   :: String,
  size    :: Int
}

data BufferPair = BufferPair {
  buffer1 :: Buffer,
  buffer2 :: Buffer,
  index   :: Int
}

data Result = Result Buffer | IMPOSSIBLE

instance Show Result where
         show IMPOSSIBLE = "IMPOSSIBLE"
         show (Result Buffer{value = value}) = value

newBuffer :: String -> Buffer
newBuffer value = Buffer {
                    value = value,
                    size  = length value
                  }

defaultBuffer :: Buffer
defaultBuffer = newBuffer ""

-- solution

main :: IO ()
main = mainLoop 1

mainLoop :: Int -> IO ()
mainLoop caseNumber = do
  eof <- isEOF
  if eof
    then return ()
    else do
      qty   <- getLine
      pairs <- getPairs (read qty) []
      putStrLn $ "Case " ++ show caseNumber ++ ": " ++ show (permutate pairs defaultBuffer defaultBuffer defaultBuffer IMPOSSIBLE)
      mainLoop $ caseNumber + 1

getPairs :: Int -> [BufferPair] -> IO [BufferPair]
getPairs 0 accBuffer          = return accBuffer
getPairs countdown accBuffer  = do
  rawPair <- getLine
  let
    [first, second] = words rawPair
    pair = BufferPair {
      buffer1 = newBuffer first,
      buffer2 = newBuffer second,
      index   = countdown
    }
    in getPairs (countdown - 1) $ pair:accBuffer

permutate :: [BufferPair] -> Buffer -> Buffer -> Buffer -> Result -> Result
permutate [] Buffer{size = 0} Buffer{size = 0} tmpBuffer result =
  chooseSolution tmpBuffer result
permutate [] Buffer{} Buffer{} Buffer{} result =
  result
permutate pairs buff1 buff2 tmpBuffer result =
  case result of
    Result resultBuffer | size tmpBuffer > size resultBuffer ->
      result
    _ | (size tmpBuffer /= 0) && (size buff1 == 0) && (size buff2 == 0) ->
      chooseSolution tmpBuffer result
    _ ->
      case rmExtraPairs pairs buff1 buff2 of
        [] ->
          result
        goodPairs ->
          foldl folder result goodPairs
          where
            folder result BufferPair{buffer1 = buffer1, buffer2 = buffer2, index = pairIndex} =
              let
                tmpBuff1 = Buffer{value = value buff1 ++ value buffer1, size = size buff1 + size buffer1}
                tmpBuff2 = Buffer{value = value buff2 ++ value buffer2, size = size buff2 + size buffer2}
                size2get = min (size tmpBuff1) (size tmpBuff2)
                (chunk1, newVal1) = smartSplit size2get tmpBuff1
                (chunk2, newVal2) = smartSplit size2get tmpBuff2
              in
                if chunk1 /= chunk2
                  then result
                  else
                    let
                      newPairs      = filter (\ BufferPair{index = index} -> index /= pairIndex) goodPairs
                      newBuff1      = Buffer{value = newVal1, size = size tmpBuff1 - size2get}
                      newBuff2      = Buffer{value = newVal2, size = size tmpBuff2 - size2get}
                      newTmpBuffer  = Buffer{value = value tmpBuffer ++ chunk1, size = size tmpBuffer + size2get}
                      in permutate newPairs newBuff1 newBuff2 newTmpBuffer result

rmExtraPairs :: [BufferPair] -> Buffer -> Buffer -> [BufferPair]
rmExtraPairs pairs Buffer{size = offset1} Buffer{size = offset2} =
  let
    folder (acc1, acc2) BufferPair{buffer1 = Buffer{size = size1}, buffer2 = Buffer{size = size2}} =
      (acc1 + size1, acc2 + size2)
    (sum1, sum2) =
      foldl folder (offset1, offset2) pairs
    filterFun BufferPair{buffer1 = Buffer{size = size1}, buffer2 = Buffer{size = size2}} =
       (size1 <= sum2) && (size2 <= sum1)
    in filter filterFun pairs

smartSplit :: Int -> Buffer -> (String, String)
smartSplit size2get Buffer{size = size, value = value} =
  if size2get == size
    then (value, "")
    else splitAt size2get value

chooseSolution :: Buffer -> Result -> Result
chooseSolution newBuffer IMPOSSIBLE = Result newBuffer
chooseSolution newBuffer (Result oldBuffer) =
  case compare (size newBuffer) (size oldBuffer) of
    LT -> Result newBuffer
    GT -> Result oldBuffer
    EQ ->
      case compare (value newBuffer) (value oldBuffer) of
        LT -> Result newBuffer
        _  -> Result oldBuffer
