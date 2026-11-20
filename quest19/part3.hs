import Data.List (foldl', minimumBy, sortOn)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)

type Counts = M.Map Char Int

-- Colours a first-layer square can turn into depending on piece behind it
reachable :: Char -> String
reachable 'C' = "CBG"
reachable 'M' = "MBR"
reachable 'Y' = "YGR"
reachable _ = "K"

bestForTrio :: String -> String -> Integer
bestForTrio trio grid = product (map toInteger (M.elems final))
  where
    options = sortOn length [opts | c <- grid, let opts = filter (`elem` trio) (reachable c), not (null opts)]
    start = M.fromList [(c, 0) | c <- trio]
    final = foldl' assign start options

    assign :: Counts -> String -> Counts
    assign counts opts = M.adjust (+ 1) (minimumBy (comparing (counts M.!)) opts) counts

main :: IO ()
main = do
  content <- readFile "quest19/input3.txt"
  let grid = filter (/= '\n') content
      colours = "CMBGRYK"
      trios = [[a, b, c] | (i, a) <- zip [0 ..] colours, (j, b) <- zip [0 ..] colours, i < j, (k, c) <- zip [0 ..] colours, j < k]
  print (maximum [bestForTrio trio grid | trio <- trios])
