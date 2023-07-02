{-# LANGUAGE Arrows #-}

import Control.Arrow
import Control.Category
import Prelude hiding (id, (.))

-- https://blog.paulme.ng/posts/2012-04-22-haskell-arrow.html

data Auto b c = Auto (b -> c)

instance Category Auto where
  id = Auto id
  Auto f . Auto g = Auto (f . g)

instance Arrow Auto where
  arr f = Auto f
  first (Auto f) = Auto $ \(x, y) -> (f x, y)

nand2 :: Auto (Bool, Bool) Bool
nand2 = arr (not . uncurry (&&))

or2 :: Auto (Bool, Bool) Bool
or2 = ((arr id &&& arr id) >>> nand2) *** ((arr id &&& arr id) >>> nand2) >>> nand2

or2' :: Auto (Bool, Bool) Bool
or2' = proc (i0, i1) -> do
  m1 <- nand2 -< (i0, i0)
  m2 <- nand2 -< (i1, i1)
  nand2 -< (m1, m2)

runAutoA :: (Bool, Bool) -> Auto (Bool, Bool) a -> IO a
runAutoA x (Auto f) = return (f x)

main = do
  let comb = [(False, False), (False, True), (True, False), (True, True)]
  result <- sequence $ flip runAutoA or2' `map` comb
  putStrLn $ show result