import Data.List (findIndex, group)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

compress :: String -> [(Char, Int)]
compress = map (\g -> (head g, length g)) . group

merge :: [(Char, Int)] -> [(Char, Int)]
merge = map combine . groupByChar
  where
    groupByChar [] = []
    groupByChar (x : xs) =
      let (same, rest) = span ((== fst x) . fst) xs
       in (x : same) : groupByChar rest
    combine xs = (fst (head xs), sum (map snd xs))

step :: [(Char, Int)] -> Maybe [(Char, Int)]
step xs =
  case findIndex (\(c, _) -> M.findWithDefault 0 c counts > 1) xs of
    Nothing -> Nothing
    Just i ->
      let c = fst (xs !! i)
          j = fromJust (findLastIndex ((== c) . fst) xs)
          (before, rest) = splitAt (i + 1) xs
          (middle, after) = splitAt (j - i) rest
       in Just $ merge $ before ++ reverse middle ++ after
  where
    counts = M.fromListWith (+) [(c, 1 :: Int) | (c, _) <- xs]

    findLastIndex p ys = case findIndex p (reverse ys) of
      Nothing -> Nothing
      Just k -> Just $ length ys - 1 - k

solve :: String -> Int
solve s = go (compress s) 0
  where
    go xs n = case step xs of
      Nothing -> n
      Just ys -> go ys (n + 1)

main :: IO ()
main = do
  content <- readFile "quest16/input1.txt"
  print $ solve content
