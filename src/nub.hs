import Data.Set as Set (empty, insert, member)

nub2 :: [Int] -> [Int]
nub2 = go Set.empty
  where
    go _ [] = []
    go cache (x : xs)
      | x `Set.member` cache = go cache xs
      | otherwise = x : go (Set.insert x cache) xs

nub3 xs = Prelude.foldr go (const []) xs Set.empty
  where
    go x pxs cache
      | x `Set.member` cache = pxs cache
      | otherwise = x : pxs (Set.insert x cache)