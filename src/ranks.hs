-- https://stackoverflow.com/a/24717685

{-# LANGUAGE ImpredicativeTypes #-}

module Main (main) where

import Prelude hiding (length)

length :: forall a. [a] -> Int
length = foldr (const $ (+) 1) 0

check :: (forall a. [a] -> Int) -> [b] -> [c] -> Bool
check f l1 l2 = f l1 == f l2

data Country
  = BigEnemy
  | MediumEnemy
  | PunyEnemy
  | TradePartner
  | Ally
  | BestAlly
  deriving (Show)

f :: (forall a. Show a => [a] -> a) -> String
f g = show $ g [BigEnemy, MediumEnemy, PunyEnemy]

main = do
  print $ check length [1] ['a', 'b']
  print $ f (\_ -> BigEnemy) -- TODO: does not work