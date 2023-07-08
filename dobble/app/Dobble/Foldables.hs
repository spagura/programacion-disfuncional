module Dobble.Foldables where

-- List that folds from back to start
newtype ReverseList a = ReverseList [a]

instance Foldable ReverseList where
    foldr f z (ReverseList []) = z
    foldr f z (ReverseList (x:xs)) = foldr f (f x z) (ReverseList xs)


-- List that folds from middle element to the sides
newtype InOutList a = InOutList [a]

instance Foldable InOutList where
    foldr f z (InOutList []) = z
    foldr f z (InOutList (x:[])) = f x z
    foldr f z (InOutList (x:xs)) = foldr f (f (last xs) (f x z)) (InOutList (init xs))
