import Data.Char (isDigit)
import qualified Data.IntMap as IM

numbers :: String -> [Integer]
numbers [] = []
numbers s = case dropWhile (not . isDigit) s of
  [] -> []
  s' ->
    let (num, rest) = span isDigit s'
     in read num : numbers rest

solve :: Integer -> Integer -> Integer
solve growth limit
  | limit <= 1 = 1
  | otherwise = go (IM.singleton 0 1) 1 0
  where
    g = fromInteger growth
    h = (g - 1) `div` 2

    go branches leaves seconds
      | leaves >= limit = seconds
      | otherwise =
          let longest = fst (IM.findMax branches)
              jump = g - longest
              shifted = IM.mapKeysMonotonic (+ (jump - 1)) branches
              splits = IM.findWithDefault 0 (g - 1) shifted
              step acc len n
                | len + 1 == g =
                    IM.insertWith (+) 1 n $ IM.insertWith (+) h (2 * n) acc
                | otherwise = IM.insertWith (+) (len + 1) n acc
              next = IM.foldlWithKey' step IM.empty shifted

              newLeaves = leaves + splits
           in go next newLeaves (seconds + fromIntegral jump)

main :: IO ()
main = do
  content <- readFile "quest09/input3.txt"
  let plants = numbers <$> lines content

  print $ sum $ [solve growth limit | [_, growth, limit] <- plants]