module Dobble.Card where 

import Dobble.Utils
import Data.List ((\\))
import System.Random (randomRIO)

type Symbol = Char
type Card = [Symbol]
type Deck = [Card]


getCard :: Int -> [Symbol] -> [Symbol] -> IO Card
getCard n exclusions requiredSymbols = do
  let availableSymbols = emojis \\ exclusions
      missingSymbols = n - length requiredSymbols
  shuffledRequiredSymbols <- shuffle requiredSymbols
  shuffledRemainingSymbols <- shuffle $ take missingSymbols availableSymbols
  return $ shuffledRequiredSymbols ++ shuffledRemainingSymbols
  where
    emojis = ['A' .. 'Z']

-- Recibe el nro de simbolos que tiene cada carta 
-- Devuelve una tripla con tres cartas que tienen solamente un simbolo en comun entre cualquier par que se tome (en un IO)
generateCards :: Int -> IO (Card, Card, Card)
generateCards n = do
    card1 <- getCard n [] []
    index0 <- randomRIO (0, n - 1)
    card2 <- getCard n card1 [card1 !! index0]
    index1 <- randomRIO (0, n - 1)
    index2 <- randomRIO (0, n - 1)
    commonCard <- getCard n (card1 ++ card2) [card1 !! index1, card2 !! index2]
    return (card1, card2, commonCard)
