{-
  Keep the shelf as a list of groups (shade, length) in a treap indexed by
  position. Each node also stores how many groups of each shade are in that
  subtree and the total tablet count
    * first/last group of a shade is found by descending on those counts
    * a flip is split / reverse / join and its cost is the segment's total
    * after flip groups of the same shade that now touch are fused
-}

import Data.Bits (shiftR, xor)
import Data.List (group)

data Counts = Counts !Int !Int !Int !Int !Int

addC :: Counts -> Counts -> Counts
addC (Counts a b c d e) (Counts a' b' c' d' e') = Counts (a + a') (b + b') (c + c') (d + d') (e + e')

getC :: Int -> Counts -> Int
getC 0 (Counts a _ _ _ _) = a
getC 1 (Counts _ b _ _ _) = b
getC 2 (Counts _ _ c _ _) = c
getC 3 (Counts _ _ _ d _) = d
getC _ (Counts _ _ _ _ e) = e

single :: Int -> Counts
single k = Counts (f 0) (f 1) (f 2) (f 3) (f 4)
  where
    f i = if i == k then 1 else 0

data Tree
  = Leaf
  | Node
      { prio :: !Int,
        shade :: !Int,
        len :: !Int,
        size :: !Int,
        counts :: !Counts,
        total :: !Int,
        flipped :: !Bool,
        left :: Tree,
        right :: Tree
      }

sizeOf :: Tree -> Int
sizeOf Leaf = 0
sizeOf t = size t

countsOf :: Tree -> Counts
countsOf Leaf = Counts 0 0 0 0 0
countsOf t = counts t

totalOf :: Tree -> Int
totalOf Leaf = 0
totalOf t = total t

node :: Int -> Int -> Int -> Tree -> Tree -> Tree
node p s n l r =
  Node
    { prio = p,
      shade = s,
      len = n,
      size = (sizeOf l + sizeOf r + 1),
      counts = (countsOf l `addC` single s `addC` countsOf r),
      total = (totalOf l + n + totalOf r),
      flipped = False,
      left = l,
      right = r
    }

-- apply a pending reversal to this node's children
settle :: Tree -> Tree
settle t
  | flipped t = t {flipped = False, left = toggle (right t), right = toggle (left t)}
  | otherwise = t

toggle :: Tree -> Tree
toggle Leaf = Leaf
toggle t = t {flipped = not (flipped t)}

merge :: Tree -> Tree -> Tree
merge Leaf b = b
merge a Leaf = a
merge a b
  | prio a >= prio b = let Node p s n _ _ _ _ l r = settle a in node p s n l (merge r b)
  | otherwise = let Node p s n _ _ _ _ l r = settle b in node p s n (merge a l) r

splitAt' :: Int -> Tree -> (Tree, Tree)
splitAt' _ Leaf = (Leaf, Leaf)
splitAt' k t
  | k <= sizeOf l = let (a, b) = splitAt' k l in (a, node p s n b r)
  | otherwise = let (a, b) = splitAt' (k - sizeOf l - 1) r in (node p s n l a, b)
  where
    Node p s n _ _ _ _ l r = settle t

-- position of the k-th (from 0) group of shade x
findKth :: Int -> Int -> Tree -> Int
findKth x k t = go k t 0
  where
    go _ Leaf _ = error "findKth: out of range"
    go i u off
      | i < inLeft = go i l off
      | shade u' == x && i == inLeft = off + sizeOf l
      | otherwise = go (i - inLeft - here) (right u') (off + sizeOf l + 1)
      where
        u' = settle u
        l = left u'
        inLeft = getC x (countsOf l)
        here = if shade u' == x then 1 else 0

-- concatenate, fusing the touching groups if they have the same shade
join :: Tree -> Tree -> Tree
join Leaf b = b
join a Leaf = a
join a b
  | shade x == shade y = merge (merge a' (node (prio x) (shade x) (len x + len y) Leaf Leaf)) b'
  | otherwise = merge a b
  where
    (a', x) = splitAt' (sizeOf a - 1) a
    (y, b') = splitAt' 1 b

step :: Tree -> Maybe (Tree, Int)
step t
  | null split = Nothing
  | otherwise = Just (join (join a (toggle m)) b, totalOf m)
  where
    cs = countsOf t
    split = [x | x <- [0 .. 4], getC x cs > 1]
    (i, c) = minimum [(findKth x 0 t, x) | x <- split]
    j = findKth c (getC c cs - 1) t
    (a, rest) = splitAt' (i + 1) t
    (m, b) = splitAt' (j - i) rest

shadeId :: Char -> Int
shadeId 'R' = 0
shadeId 'G' = 1
shadeId 'B' = 2
shadeId 'Y' = 3
shadeId _ = 4

hash :: Int -> Int
hash x = z `xor` (z `shiftR` 27)
  where
    -- SplitMix64 hash func:
    y = (x + 0x9e3779b97f4a7c15) * 0xbf58476d1ce4e5b9
    z = (y `xor` (y `shiftR` 30)) * 0x94d049bb133111eb

build :: [(Int, Int)] -> Tree
build gs = foldl merge Leaf [node (hash i) s n Leaf Leaf | (i, (s, n)) <- zip [0 ..] gs]

solve :: String -> Int
solve s = go (build [(shadeId (head g), length g) | g <- group s]) 0
  where
    go t acc = case step t of
      Nothing -> acc
      Just (t', cost) -> go t' $! acc + cost

main :: IO ()
main = do
  content <- readFile "quest16/input3.txt"
  print $ solve $ concat $ replicate 100 content
