import Data.Foldable (foldl')
import qualified Data.Map as Map

type Position = (Int, Int)

type Direction = (Int, Int)

type Shadows = Map.Map Position Int

data Cell = Ent Int | Seed Int
  deriving (Show, Eq)

type Field = Map.Map Position Cell

directions :: [Direction]
directions = [(1, 1), (1, -1), (0, -2), (-1, -1), (-1, 1), (0, 2)]

parseGrid :: [String] -> Field
parseGrid rows = Map.fromList [((r, c), Ent (read [ch])) | (r, row) <- zip [0 ..] rows, (c, ch) <- zip [0 ..] row, ch >= '1' && ch <= '9']

castShadow :: Direction -> Position -> Int -> [((Int, Int), Int)]
castShadow (dr, dc) (r, c) height = [((r + step * dr, c + step * dc), height - step) | step <- [1 .. height - 1]]

buildShadows :: Direction -> Field -> Shadows
buildShadows direction field = foldl' addShadow Map.empty (Map.toList field)
  where
    addShadow shadows (pos, cell) =
      case cell of
        Ent height ->
          foldl'
            (\m (p, h) -> Map.insertWith max p h m)
            shadows
            (castShadow direction pos height)
        Seed _ -> shadows

advanceSeeds :: Field -> Field
advanceSeeds =
  Map.map advance
  where
    advance cell =
      case cell of
        Ent height -> Ent height
        Seed 0 -> Ent 1
        Seed remaining -> Seed (remaining - 1)

runRound :: Direction -> Field -> (Field, Integer, Integer)
runRound direction field =
  let shadows = buildShadows direction field
      (field', energy, warriors) =
        foldl'
          (growEnt shadows)
          (Map.empty, 0, 0)
          (Map.toList field)
   in (field', energy, warriors)
  where
    growEnt shadows (newField, energy, warriors) (pos, cell) =
      case cell of
        Seed remaining ->
          ( Map.insert pos (Seed remaining) newField,
            energy,
            warriors
          )
        Ent height ->
          let shadow = Map.findWithDefault 0 pos shadows
           in if height <= shadow
                then
                  (Map.insert pos (Ent height) newField, energy, warriors)
                else
                  if height == 9
                    then
                      (Map.insert pos (Seed 3) newField, energy + 1, warriors + 1)
                    else
                      (Map.insert pos (Ent (height + 1)) newField, energy + 1, warriors)

simulate :: Integer -> Field -> Integer
simulate target initialField = go 0 initialField 0 0
  where
    go roundNumber field totalEnergy warriors
      | warriors >= target = totalEnergy
      | otherwise =
          let direction = directions !! (roundNumber `mod` 6)
              readyField = advanceSeeds field
              (field', energy, newWarriors) = runRound direction readyField
           in go (roundNumber + 1) field' (totalEnergy + energy) (warriors + newWarriors)

solve :: [String] -> Integer
solve rows =
  let field = parseGrid rows
   in simulate 100000 field

main :: IO ()
main = do
  rows <- lines <$> readFile "quest08/input3.txt"
  print $ solve rows
