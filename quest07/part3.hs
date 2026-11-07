import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- (initial energy, outgoing connections)
type Cloud = (Integer, [Int])

-- Map cloud ID to cloud info
type Sky = Map.Map Int Cloud

-- Map (cloud ID, impulse strength) to num impulses
type Impulses = Map.Map (Int, Int) Integer

parseRecord :: String -> (Int, Cloud)
parseRecord line =
  let fields = splitOn ", " line

      getValue field = drop 1 (dropWhile (/= '=') field)

      cloudID = read (getValue (fields !! 0))
      energy = read (getValue (fields !! 1))

      connections =
        map read (splitOn "," (getValue (fields !! 2)))
   in (cloudID, (energy, connections))

-- Build a map containing all clouds.
buildSky :: [String] -> Sky
buildSky = Map.fromList . map parseRecord

processStage ::
  Sky ->
  Set.Set Int ->
  Impulses ->
  Map.Map Int Integer ->
  (Map.Map Int Integer, Impulses)
processStage sky exploded pending energies =
  foldl'
    processOne
    (energies, Map.empty)
    (Map.toList pending)
  where
    processOne
      (energyMap, nextImpulses)
      ((cloudID, strength), count)
        | Set.member cloudID exploded =
            (energyMap, nextImpulses)
        | otherwise =
            let energyMap' =
                  Map.insertWith
                    (+)
                    cloudID
                    (count * fromIntegral strength)
                    energyMap

                normalImpulses =
                  if strength > 1
                    then
                      let (_, connections) =
                            Map.findWithDefault (0, []) cloudID sky
                       in foldl'
                            ( \m next ->
                                Map.insertWith
                                  (+)
                                  (next, strength - 1)
                                  count
                                  m
                            )
                            nextImpulses
                            connections
                    else
                      nextImpulses
             in (energyMap', normalImpulses)

-- Find every cloud that reached threshold
findExploded ::
  Integer ->
  Sky ->
  Set.Set Int ->
  Map.Map Int Integer ->
  [Int]
findExploded limit sky exploded energies =
  [ cloudID
    | cloudID <- Map.keys sky,
      not (Set.member cloudID exploded),
      Map.findWithDefault 0 cloudID energies >= limit
  ]

explosionImpulses :: Sky -> [Int] -> Impulses
explosionImpulses sky explodedClouds =
  foldl' sendExplosion Map.empty explodedClouds
  where
    sendExplosion impulses cloudID =
      let (_, connections) =
            Map.findWithDefault (0, []) cloudID sky
       in foldl'
            ( \m next ->
                Map.insertWith (+) (next, 10) 1 m
            )
            impulses
            connections

-- Run propagation process for one starting cloud
simulate :: Integer -> Sky -> Int -> Integer
simulate limit sky start =
  let initialEnergies =
        Map.map fst sky

      initialImpulses =
        Map.singleton (start, 10) 1
   in go initialImpulses initialEnergies Set.empty
  where
    go pending energies exploded
      | Map.null pending =
          Map.foldlWithKey'
            ( \total cloudID energy ->
                if Set.member cloudID exploded
                  then total
                  else total + energy
            )
            0
            energies
      | otherwise =
          let (energies', normalImpulses) =
                processStage sky exploded pending energies
              newlyExploded =
                findExploded limit sky exploded energies'
              exploded' =
                foldl'
                  (flip Set.insert)
                  exploded
                  newlyExploded
              extraImpulses =
                explosionImpulses sky newlyExploded
              nextImpulses =
                Map.unionWith
                  (+)
                  normalImpulses
                  extraImpulses
           in go nextImpulses energies' exploded'

solve :: Integer -> [String] -> Integer
solve limit records =
  let sky = buildSky records

      possibleStarts = Map.keys sky

      results =
        [ simulate limit sky start
          | start <- possibleStarts
        ]
   in maximum results

main :: IO ()
main = do
  records <- lines <$> readFile "quest07/input3.txt"
  print $ solve 1000000 records
