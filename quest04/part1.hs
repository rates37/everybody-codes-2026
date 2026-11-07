import Data.List (sortOn)
import qualified Data.Map as Map

main :: IO ()
main = do
  content <- fmap lines (readFile "quest04/input1.txt")
  let -- Parse every line into a list of fields: "id=5 quest=1 part=1 time=396" -> [5,1,1,396]
      records = map parseLine content
      -- Turn each line into a list of its values
      parseLine line =
        let fields = map (read . drop 1 . dropWhile (/= '=')) (words line)
         in fields

      -- Build sorted list of per-id time totals
      totals = map sum . sortOn sum . Map.elems $ Map.fromListWith (++) [(head r, [last r]) | r <- records]

  print $ sum $ zipWith (*) [1 ..] totals
