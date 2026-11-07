import Data.Function (on)
import Data.List (groupBy, sortOn)
import qualified Data.Map as Map

type Record = (Integer, Integer, Integer, Integer)

main :: IO ()
main = do
  content <- fmap lines (readFile "quest04/input3.txt")
  let records = map parseLine content
      parseLine line =
        let fields = map (read . drop 1 . dropWhile (/= '=')) (words line)
         in case fields of
              [a, b, c, d] -> (a, b, c, d)
              _ -> error "ruh roh"

      quests = Map.fromListWith (++) [(q, [(i, p, t)]) | (i, q, p, t) <- records]

      points = concatMap scoreQuest (Map.elems quests)

      scoreQuest questRecords = concatMap scorePart [1 .. 3]
        where
          scorePart part =
            let completed = Map.fromList [(i, t) | (i, p, t) <- questRecords, p == part]
                cumulative = [(i, sum [time | (j, p, time) <- questRecords, j == i, p <= part]) | i <- Map.keys completed]
                ranked = sortOn snd cumulative
                groups = groupBy ((==) `on` snd) ranked
                award group =
                  let position = sum (map length previousGroups) + 1
                      pointAwarded = max 0 (part * 50 - position + 1)
                   in [(i, pointAwarded) | i <- map fst group]
                  where
                    previousGroups = takeWhile (/= group) groups
             in concatMap award groups

      totals = Map.fromListWith (+) points
      finalRanking = sortOn (negate . snd) (Map.toList totals)
      groups = groupBy ((==) `on` snd) finalRanking
      positions = concat (zipWith assign (scanl (+) 1 (map length groups)) groups)
        where
          assign pos g = replicate (length g) pos
      scores = map snd finalRanking

  print $ sum $ zipWith (*) positions scores
