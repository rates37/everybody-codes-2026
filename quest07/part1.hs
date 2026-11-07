import Data.List (maximumBy)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)

-- (initial energy, outgoing connections)
type Cloud = (Int, [Int])

-- Map cloud ID to cloud info
type Sky = Map.Map Int Cloud

-- e.g. "id=1, energy=5, connections=2,5,7" -> (1, (5, [2,5,7]))
parseRecord :: String -> (Int, Cloud)
parseRecord line =
  let fields = splitOn ", " line
      getValue field = drop 1 (dropWhile (/= '=') field)
      cloudID = read (getValue (fields !! 0))
      energy = read (getValue (fields !! 1))
      connections = map read (splitOn "," (getValue (fields !! 2)))
   in (cloudID, (energy, connections))

-- Build a map containing all clouds.
buildSky :: [String] -> Sky
buildSky = Map.fromList . map parseRecord

-- Find the cloud with the largest num of outgoing connections
findStartingCloud :: Sky -> Int
findStartingCloud sky = fst $ maximumBy (comparing (length . snd . snd)) (Map.toList sky)

-- Propagate impulses through network
-- Returns a map containing the total additional energy for each cloud
propagate :: Sky -> Int -> Map.Map Int Int
propagate sky start = go [(start, 5)] Map.empty
  where
    go [] energyGained = energyGained
    go ((cloudID, strength) : stack) energyGained =
      let energyGained' = Map.insertWith (+) cloudID strength energyGained
          (_, connections) = Map.findWithDefault (0, []) cloudID sky
          newImpulses =
            if strength > 1
              then map (\next -> (next, strength - 1)) connections
              else []
       in go (newImpulses ++ stack) energyGained'

solve :: [String] -> Int
solve records =
  let sky = buildSky records
      start = findStartingCloud sky
      energyGained = propagate sky start
      finalEnergies =
        [ initialEnergy + Map.findWithDefault 0 cloudID energyGained
          | (cloudID, (initialEnergy, _)) <- Map.toList sky
        ]
   in sum finalEnergies

main :: IO ()
main = do
  records <- lines <$> readFile "quest07/input1.txt"
  print $ solve records
