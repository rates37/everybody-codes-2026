import Data.Foldable (foldl')
import Data.List (sortOn)
import qualified Data.Map as Map

type Position = (Int, Int)

type Direction = (Int, Int)

directions :: [Direction]
directions = [(1, 1), (1, -1), (0, -2), (-1, -1), (-1, 1), (0, 2)]

parseGrid :: [String] -> [(Position, Int)]
parseGrid rows = [((r, c), read [ch]) | (r, row) <- zip [0 ..] rows, (c, ch) <- zip [0 ..] row, ch >= '1' && ch <= '9']

castShadow :: Direction -> Position -> Int -> [((Int, Int), Int)]
castShadow (dr, dc) (r, c) height = [((r + dr * step, c + dc * step), height - step) | step <- [1 .. height - 1]]

solveRound :: [String] -> Direction -> Int
solveRound rows direction =
  let ents = sortOn (\((r, c), _) -> fst direction * r + snd direction * c) (parseGrid rows)
      (_, energyCount) = foldl' processEnt (Map.empty, 0) ents
   in energyCount
  where
    processEnt (shadows, count) (pos, height) =
      let shadow = Map.findWithDefault 0 pos shadows
          count' = if height > shadow then count + 1 else count
          newShadows = castShadow direction pos height

          shadows' = foldl' (\m (p, h) -> Map.insertWith max p h m) shadows newShadows
       in (shadows', count')

solve :: [String] -> Int
solve rows = sum [solveRound rows direction | direction <- directions]

main :: IO ()
main = do
  rows <- lines <$> readFile "quest08/input2.txt"
  print $ solve rows
