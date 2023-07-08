module Dobble.Foldables where

newtype ReverseList a = ReverseList [a]


instance Foldable ReverseList where
    foldr f z (ReverseList []) = z
    foldr f z (ReverseList (x:xs)) = foldr f (f x z) (ReverseList xs)
