import Data.Foldable (foldl')
import qualified Data.Map as Map

type Position = (Int, Int)

parseGrid :: [String] -> [(Position, Int)]
parseGrid rows = [((r, c), read [ch]) | (r, row) <- zip [0 ..] rows, (c, ch) <- zip [0 ..] row, ch >= '1' && ch <= '9']

castShadow :: Position -> Int -> [((Int, Int), Int)]
castShadow (r, c) height = [((r + step, c + step), height - step) | step <- [1 .. height - 1]]

solve :: [String] -> Int
solve rows =
  let ents = parseGrid rows
      (_, energyCount) = foldl' processEnt (Map.empty, 0) ents
   in energyCount
  where
    processEnt (shadows, count) (pos, height) =
      let shadow = Map.findWithDefault 0 pos shadows
          count' = if height > shadow then count + 1 else count
          newShadows = castShadow pos height
          shadows' = foldl' (\m (p, h) -> Map.insertWith max p h m) shadows newShadows
       in (shadows', count')

main :: IO ()
main = do
  rows <- lines <$> readFile "quest08/input1.txt"
  print $ solve rows
