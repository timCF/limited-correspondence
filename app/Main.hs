module Main where

import System.IO (isEOF)
import Debug.Trace
data Buffer = Buffer {
  value   :: String,
  size    :: Int
}

data BufferPair = BufferPair {
  buffer1 :: Buffer,
  buffer2 :: Buffer,
  index   :: Int
}

defaultBuffer = Buffer {
  value = "",
  size  = 0
}

impossibleBuffer = Buffer {
  value = "IMPOSSIBLE",
  size  = -1
}

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
      putStrLn $ "Case " ++ show caseNumber ++ ": " ++ value (permutate pairs defaultBuffer defaultBuffer defaultBuffer impossibleBuffer)
      mainLoop $ caseNumber + 1

getPairs :: Int -> [BufferPair] -> IO [BufferPair]
getPairs 0 accBuffer          = return accBuffer
getPairs countdown accBuffer  = do
  rawPair <- getLine
  let
    [first, second] = words rawPair
    pair = BufferPair {
      buffer1 = Buffer {
        value = first,
        size  = length first
      },
      buffer2 = Buffer {
        value = second,
        size  = length second
      },
      index   = countdown
    }
    in getPairs (countdown - 1) $ pair:accBuffer

permutate :: [BufferPair] -> Buffer -> Buffer -> Buffer -> Buffer -> Buffer
permutate [] Buffer{size = 0} Buffer{size = 0} tmpBuffer accBuffer =
  chooseSolution tmpBuffer accBuffer
permutate [] Buffer{} Buffer{} Buffer{} accBuffer =
  accBuffer
permutate pairs buff1 buff2 tmpBuffer accBuffer
  | (size tmpBuffer > size accBuffer) && (size accBuffer /= -1) =
      accBuffer
  | (size tmpBuffer /= 0) && (size buff1 == 0) && (size buff2 == 0) =
      chooseSolution tmpBuffer accBuffer
  | otherwise =
      case rmExtraPairs pairs buff1 buff2 of
        [] -> accBuffer
        goodPairs -> foldl folder accBuffer goodPairs
  where
    folder accBuffer BufferPair{buffer1 = buffer1, buffer2 = buffer2, index = pairIndex} =
      let
        tmpBuff1 = Buffer{value = value buff1 ++ value buffer1, size = size buff1 + size buffer1}
        tmpBuff2 = Buffer{value = value buff2 ++ value buffer2, size = size buff2 + size buffer2}
        size2get = min (size tmpBuff1) (size tmpBuff2)
        (chunk1, newVal1) = smartSplit size2get tmpBuff1
        (chunk2, newVal2) = smartSplit size2get tmpBuff2
      in
        if chunk1 /= chunk2
          then accBuffer
          else
            let
              newPairs      = filter (\ BufferPair{index = index} -> index /= pairIndex) pairs
              newBuff1      = Buffer{value = newVal1, size = size tmpBuff1 - size2get}
              newBuff2      = Buffer{value = newVal2, size = size tmpBuff2 - size2get}
              newTmpBuffer  = Buffer{value = value tmpBuffer ++ chunk1, size = size tmpBuffer + size2get}
              in permutate newPairs newBuff1 newBuff2 newTmpBuffer accBuffer

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

chooseSolution :: Buffer -> Buffer -> Buffer
chooseSolution newBuffer Buffer{value = "IMPOSSIBLE"} = newBuffer
chooseSolution newBuffer oldBuffer    =
  case compare (size newBuffer) (size oldBuffer) of
    LT -> newBuffer
    GT -> oldBuffer
    EQ ->
      case compare (value newBuffer) (value oldBuffer) of
        LT -> newBuffer
        _  -> oldBuffer
