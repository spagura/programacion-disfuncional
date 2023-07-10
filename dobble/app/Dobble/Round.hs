{-# LANGUAGE InstanceSigs #-}
module Dobble.Round where

import Data.Foldable (Foldable, foldl)
import Control.Monad.Writer
import Dobble.Utils
import Dobble.Foldables
import Dobble.Card

data MatchResult = Match | NoMatch deriving Show

instance Semigroup MatchResult where
    (<>) :: MatchResult -> MatchResult -> MatchResult
    Match <> _ = Match
    _ <> Match = Match
    _ <> _ = NoMatch

instance Monoid MatchResult where
    mempty = NoMatch

computeMatchResult :: Symbol -> Symbol -> MatchResult
computeMatchResult s1 s2
    | s1 == s2 = Match
    | otherwise = NoMatch

matchSymbol :: Symbol -> Card -> MatchResult
matchSymbol symbol card = foldMap (\ a -> computeMatchResult a symbol) card

matchCard :: (Foldable t) => t Symbol -> Card -> Writer [Symbol] MatchResult
matchCard playerCard commonCard = foldl (\acc x -> do
    current <- acc
    case (current) of
        (Match) -> return Match
        (NoMatch) -> do
            tell [x]
            return $ let matchResult = matchSymbol x commonCard
                in current <> matchResult) (return NoMatch) playerCard

playRound :: Card -> Card -> Card -> (Card, Card)
playRound c1 c2 cCommon = let
    c1' = InOutList c1 -- or other foldable implementation
    c2' = ReverseList c2 -- or other foldable implementation
    in  mapTuple (snd . runWriter)(matchCard c1' cCommon, matchCard c2' cCommon)
