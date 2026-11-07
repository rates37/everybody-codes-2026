import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)

type Ghost = (Int, Int, Char, Int)

clockwise :: Char -> Char
clockwise '>' = 'v'
clockwise 'v' = '<'
clockwise '<' = '^'
clockwise '^' = '>'
clockwise c = c

reverseDir :: Char -> Char
reverseDir '>' = '<'
reverseDir 'v' = '^'
reverseDir '<' = '>'
reverseDir '^' = 'v'
reverseDir c = c

move :: [String] -> Ghost -> Maybe Ghost
move grid (r, c, d, remaining) =
  let (nr, nc) = case d of
        '>' -> (r, c + 1)
        'v' -> (r + 1, c)
        '<' -> (r, c - 1)
        '^' -> (r - 1, c)
        _ -> (r, c)
   in if grid !! nr !! nc == '#'
        then
          if remaining == 1
            then Nothing
            else Just (r, c, clockwise d, remaining - 1)
        else Just (nr, nc, d, remaining)

roundSim :: [String] -> [Ghost] -> [Ghost]
roundSim grid ghosts =
  let moved = mapMaybe (move grid) ghosts

      groups =
        M.fromListWith
          (+)
          [((r, c), 1 :: Int) | (r, c, _, _) <- moved]

      collide (r, c, d, remaining) =
        if M.findWithDefault 0 (r, c) groups > 1
          then
            if remaining == 1
              then Nothing
              else Just (r, c, reverseDir d, remaining - 1)
          else Just (r, c, d, remaining)
   in mapMaybe collide moved

simulate :: [String] -> [Ghost] -> Int
simulate grid = go 0
  where
    go acc [] = acc
    go acc ghosts = go (acc + 1) (roundSim grid ghosts)

main :: IO ()
main = do
  grid <- lines <$> readFile "quest10/input2.txt"

  print $ simulate grid [(r, c, d, 10000) | (r, row) <- zip [0 ..] grid, (c, d) <- zip [0 ..] row, d `elem` "<>^v"]
