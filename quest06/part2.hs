import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Recipe = Map.Map String [String]

parseRecord :: String -> (String, [String])
parseRecord line =
  let (item, rest) = break (== ':') line
      ingredients = drop 1 rest
   in (item, splitOn "," ingredients)

buildRecipe :: [String] -> Recipe
buildRecipe records = Map.fromListWith (++) [(ingredient, [product]) | record <- records, let (product, ingredients) = parseRecord record, ingredient <- ingredients]

reachable :: Recipe -> String -> Set.Set String
reachable recipe start = go Set.empty [start]
  where
    go visited [] = visited
    go visited (current : stack)
      | Set.member current visited = go visited stack
      | otherwise =
          let visited' = Set.insert current visited
              products = Map.findWithDefault [] current recipe
           in go visited' (products ++ stack)

solve :: [String] -> Int
solve records = Set.size $ Set.delete "NULL" $ reachable (buildRecipe records) "NULL"

main :: IO ()
main = do
  records <- lines <$> readFile "quest06/input2.txt"
  print $ solve records