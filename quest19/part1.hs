import Data.List (foldl', sortBy)
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

main :: IO ()
main = do
  contents <- readFile "quest19/input1.txt"
  let grids = filter (not . null) (lines contents)
      (firstGrid, secondGrid) = splitAt (length grids `div` 2) grids
      mixedGrid = zipWith (zipWith mix) firstGrid secondGrid
      counts = foldl' countColour [] (concat mixedGrid)
      countColour acc col = case lookup col acc of
        Just _ -> map (\(c, x) -> if c == col then (c, x + 1) else (c, x)) acc
        Nothing -> (col, 1) : acc
      frequencies = sortBy (flip $ comparing snd) counts
  print $ product $ map snd $ take 3 frequencies
