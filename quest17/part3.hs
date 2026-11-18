import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified Data.Set as S

type Point = (Int, Int)

type Edge = (Point, Point)

-- check if solid
solid :: [String] -> Int -> Int -> Int -> Int -> Bool
solid grid h w r c = r >= 0 && r < h && c >= 0 && c < w && grid !! r !! c == '#'

-- Find exposed edges, clockwise
boundary :: [String] -> Int -> Int -> [Edge]
boundary grid h w =
  [((c, r), (c + 1, r)) | r <- [0 .. h - 1], c <- [0 .. w - 1], solid grid h w r c, not (solid grid h w (r - 1) c)]
    ++ [((c + 1, r), (c + 1, r + 1)) | r <- [0 .. h - 1], c <- [0 .. w - 1], solid grid h w r c, not (solid grid h w r (c + 1))]
    ++ [((c + 1, r + 1), (c, r + 1)) | r <- [0 .. h - 1], c <- [0 .. w - 1], solid grid h w r c, not (solid grid h w (r + 1) c)]
    ++ [((c, r + 1), (c, r)) | r <- [0 .. h - 1], c <- [0 .. w - 1], solid grid h w r c, not (solid grid h w r (c - 1))]

direction :: Edge -> Point
direction ((x1, y1), (x2, y2)) = (x2 - x1, y2 - y1)

-- Prefer turning right at corners if diagonal touching then straight, left, back
turnRank :: Point -> Point -> Int
turnRank (dx, dy) (nx, ny)
  | (nx, ny) == (-dy, dx) = 0
  | (nx, ny) == (dx, dy) = 1
  | (nx, ny) == (dy, -dx) = 2
  | otherwise = 3

trace :: [Edge] -> Point -> Point -> Int
trace edges start target = go start Nothing S.empty 0
  where
    go current previous visited dist
      | current == target = dist
      | otherwise =
          let options = [e | e@(from, _) <- edges, from == current, S.notMember e visited]

              next = case previous of
                Nothing -> head options
                Just dir -> minimumBy (comparing (turnRank dir . direction)) options

              nextPoint = snd next
              nextDir = direction next
           in go nextPoint (Just nextDir) (S.insert next visited) (dist + 1)

main :: IO ()
main = do
  input <- readFile "quest17/input3.txt"
  let grid = lines input
      h = length grid
      w = length (head grid)
      edges = boundary grid h w
      perimeter = length edges
      d = trace edges (0, 0) (w, h)
  print $ min d $ perimeter - d