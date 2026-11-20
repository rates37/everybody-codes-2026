import Data.List (foldl', permutations, sortBy)
import qualified Data.Map as Map
import Data.Ord (comparing)

mix :: Char -> Char -> Char
mix 'K' _ = 'K'
mix _ 'K' = 'K'
mix 'C' 'C' = 'C'
mix 'M' 'M' = 'M'
mix 'Y' 'Y' = 'Y'
mix 'C' 'M' = 'B'
mix 'M' 'C' = 'B'
mix 'C' 'Y' = 'G'
mix 'Y' 'C' = 'G'
mix 'M' 'Y' = 'R'
mix 'Y' 'M' = 'R'
mix _ _ = undefined

convert :: [Char] -> Char -> Char
convert mapping digit = mapping !! (read [digit] - 1)

calculate :: [String] -> [String] -> [Char] -> Int
calculate firstGrid secondGrid mapping =
  let convertedGrid = map (map $ convert mapping) secondGrid
      mixedGrid = zipWith (zipWith mix) firstGrid convertedGrid
      counts = foldl' (\m c -> Map.insertWith (+) c 1 m) Map.empty (concat mixedGrid)
      frequencies = sortBy (flip $ comparing snd) (Map.toList counts)
   in product $ map snd $ take 3 frequencies

main :: IO ()
main = do
  contents <- readFile "quest19/input2.txt"
  let grids = filter (not . null) (lines contents)
      (firstGrid, secondGrid) = splitAt (length grids `div` 2) grids
      assignments = permutations "CMYK"
      results = map (calculate firstGrid secondGrid) assignments
  print $ maximum results
