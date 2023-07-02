-- https://serokell.io/blog/universal-and-existential-quantification
{-# LANGUAGE ScopedTypeVariables #-}

module Scoped where

example0 :: a -> [a] -> [a]
example0 x xs = pair ++ xs
  where
    pair = [x, x]

example1 :: forall a. a -> [a] -> [a]
example1 x xs = pair ++ xs
  where
    pair :: [a]
    pair = [x, x]