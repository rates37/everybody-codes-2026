import Data.List (sortOn)
import qualified Data.Map as Map

main :: IO ()
main = do
  content <- fmap lines (readFile "quest04/input2.txt")
  let records = map parseLine content
      parseLine line =
        let fields = map (read . drop 1 . dropWhile (/= '=')) (words line)
         in fields

      participants = Map.fromListWith (++) [(head r, [last r]) | r <- records]

      stats = [(length times, sum times) | times <- Map.elems participants]

      ranked = sortOn (\(cnt, ttl) -> (-cnt, ttl)) stats

      totals = map snd ranked

  print $ sum $ zipWith (*) [1 ..] totals
