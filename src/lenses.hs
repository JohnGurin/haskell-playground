data Lens s a = Lens
  { get :: s -> a,
    set :: a -> s -> s
  }

fstL :: Lens (a, b) a
fstL =
  Lens
    { get = \(x, y) -> x,
      set = \x (_, y) -> (x, y)
    }

sndL :: Lens (a, b) b
sndL =
  Lens
    { get = \(x, y) -> y,
      set = \y (x, _) -> (x, y)
    }

update :: Lens s a -> (a -> a) -> s -> s
update l f s = set l (f (get l s)) s

compose :: Lens b c -> Lens a b -> Lens a c
compose bc ab =
  Lens
    { get = get bc . get ab,
      set = update ab . set bc
    }

updateSub = update fstL (+ 1) (1, 2)

main = putStrLn . show $ updateSub
