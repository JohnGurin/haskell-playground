type Algebra f a = f a -> a

type Coalgebra f a = a -> f a

data Fix f = Fix {unfix :: f (Fix f)}

in' :: Algebra a (Fix a)
in' = Fix

out :: Coalgebra a (Fix a)
out = unfix

cata :: Functor f => Algebra f a -> (r -> f r) -> r -> a
cata alg out = alg . fmap (cata alg out) . out

ana :: Functor f => Coalgebra f a -> (f r -> r) -> a -> r
ana coalg in' = in' . fmap (ana coalg in') . coalg

holy :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
holy alg coalg = alg . fmap (holy alg coalg) . coalg

{-
data List = Nil | Cons Int List

out:: List -> ListF List
out Nil = NilF
out (Cons h t) = ConsF h t

in' :: ListF List -> List
in' NilF = Nil
in' (ConsF h t) = Cons h t

range :: Int -> List
range n = if n > 0 then Cons n (range (n - 1)) else Nil
-}

data ListF a = NilF | ConsF Int a

instance Functor ListF where
  fmap _ NilF = NilF
  fmap f (ConsF h t) = ConsF h $ f t

multAlg :: Algebra ListF Int
multAlg NilF = 1
multAlg (ConsF h t) = h * t

rangeCoalg :: Coalgebra ListF Int
rangeCoalg n = if n > 0 then ConsF n (n - 1) else NilF

mult = cata multAlg out

range = ana rangeCoalg in'

fact = holy multAlg rangeCoalg

xfs = Fix $ ConsF 6 $ Fix $ ConsF 3 $ Fix $ ConsF 2 $ Fix NilF