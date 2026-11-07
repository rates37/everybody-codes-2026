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
solve growth limit = go (IM.singleton 0 1) 0 0
  where
    g = fromInteger growth
    h = (g - 1) `div` 2

    go branches size seconds
      | size >= limit = seconds
      | otherwise =
          let count = sum (IM.elems branches)
              step acc len n
                | len + 1 == g =
                    IM.insertWith (+) 1 n $ IM.insertWith (+) h (2 * n) acc
                | otherwise = IM.insertWith (+) (len + 1) n acc
              next = IM.foldlWithKey' step IM.empty branches
              splits = IM.findWithDefault 0 (g - 1) branches
              newSize = size + count + splits
           in go next newSize (seconds + 1)

main :: IO ()
main = do
  content <- readFile "quest09/input1.txt"
  let plants = numbers <$> lines content

  print $ sum $ [solve growth limit | [_, growth, limit] <- plants]