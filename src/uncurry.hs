-- https://stackoverflow.com/a/71175317/4350316

import Data.Map
  ( Map,
    foldMapWithKey,
  )

intersectedToList ks k v
  | k `elem` ks = [v]
  | otherwise = []

keyIntersectedVals :: Maybe [Int]
keyIntersectedVals =
  uncurry (foldMapWithKey . intersectedToList) <$> sequenceT (mkeys, mmap)
{-

intersectedToList
  :: (Foldable t, Eq a1) => t a1 -> a1 -> a2 -> [a2]
foldMapWithKey
  :: Monoid m => (k -> a -> m) -> Map k a -> m
(foldMapWithKey . intersectedToList)
  :: (Foldable t, Eq k) => t k -> Map k a -> [a]
uncurry (foldMapWithKey . intersectedToList)
  :: (Foldable t, Eq k) => (t k, Map k a) -> [a]

-}

-- aux --

sequenceT :: (Maybe [String], Maybe (Map String Int)) -> Maybe ([String], Map String Int)
sequenceT = undefined

mmap :: Maybe (Map String Int)
mmap = undefined

mkeys :: Maybe [String]
mkeys = undefined
