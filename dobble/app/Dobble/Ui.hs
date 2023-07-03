{-# LANGUAGE InstanceSigs #-}
module Dobble.Ui where

import qualified Brick.Main as BM
import qualified Brick.Types as T
import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes as VA
import Brick (Widget, (<+>), str, withBorderStyle, joinBorders)
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Core (padAll, vBox, hBox, padRight, padLeft, padTopBottom, padLeftRight, padBottom, Padding(Pad), (<=>))
import Brick.Widgets.Border (borderWithLabel, vBorder, hBorder)
import Brick.Widgets.Border.Style (unicode)
import System.Random (randomRIO)
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Data.List ((\\))
import Data.Foldable (Foldable, foldl)
import Control.Monad.Writer

type Symbol = Char
type Card = [Symbol]
type Deck = [Card]

-- Funcion para hacerle shuffle a una lista
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle items = do
    index <- randomRIO (0, length items - 1)
    rest <- shuffle (take index items ++ drop (index + 1) items)
    return (items !! index : rest)

-- Recibe el nro de simbolos que tiene cada carta 
-- Devuelve una tripla con tres cartas que tienen solamente un simbolo en comun entre cualquier par que se tome (en un IO)
generateCards :: Int -> IO (Card, Card, Card)
generateCards n = do
    card1 <- getNSymbols n [] []
    index0 <- randomRIO (0, n - 1)
    card2 <- getNSymbols n card1 [card1 !! index0]
    index1 <- randomRIO (0, n - 1)
    index2 <- randomRIO (0, n - 1)
    commonCard <- getNSymbols n (card1 ++ card2) [card1 !! index1, card2 !! index2]
    return (card1, card2, commonCard)

getNSymbols :: Int -> [Symbol] -> [Symbol] -> IO [Symbol]
getNSymbols n exclusions requiredSymbols = do
  let availableSymbols = emojis \\ exclusions
      missingSymbols = n - length requiredSymbols
  shuffledRequiredSymbols <- shuffle requiredSymbols
  shuffledRemainingSymbols <- shuffle $ take missingSymbols availableSymbols
  return $ shuffledRequiredSymbols ++ shuffledRemainingSymbols

  where
    emojis = ['A' .. 'Z']

-- Chquea que dos cartas tengan 1 simbolo en comun
symbolsInCommon :: Card -> Card -> Bool
symbolsInCommon card1 card2 = length (filter (`elem` card2) card1) == 1

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

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

play :: Card -> Card -> Card -> (Card, Card)
play c1 c2 cCommon = let
    c1' = c1 -- or other foldable implementation
    c2' = c2 -- or other foldable implementation
    in  mapTuple (snd . runWriter)(matchCard c1' cCommon, matchCard c2' cCommon)

data GameState = GameState {
    cardPlayer1 :: Card,
    cardPlayer2 :: Card,
    cardCommon :: Card,
    player1Points :: Int,
    player2Points :: Int,
    usedPlayer1 :: Card,
    usedPlayer2 :: Card
} deriving (Show, Read, Eq, Ord)

initState :: IO GameState
initState = do
    (card1, card2, card3) <- generateCards 7
    return $ GameState {
        cardPlayer1 = card1,
        cardPlayer2 = card2,
        cardCommon = card3,
        player1Points = 0,
        player2Points = 0,
        usedPlayer1 = [],
        usedPlayer2 = []
    }

findWinner :: Card -> Card -> Int
findWinner c1 c2
    |  l1 == l2 = 0 -- tied, no winner
    |  l1 < l2 = 1
    |  l1 > l2 = 2
    where l1 = length c1
          l2 = length c2

updateState :: GameState -> IO GameState
updateState s = do
    let (used1, used2) = play (cardPlayer1 s) (cardPlayer2 s) (cardCommon s)
    (card1, card2, card3) <- generateCards 7
    freshState <- initState
    let updatedGameState = incrementPlayerPoints (findWinner used1 used2) s
    return (freshState { usedPlayer1 = used1, usedPlayer2 = used2, cardPlayer1 = card1, cardPlayer2 = card2, cardCommon = card3, player1Points = player1Points updatedGameState, player2Points = player2Points updatedGameState })

dobbleMain :: IO ()
dobbleMain = do
    BM.defaultMain app =<< initState
    return ()

app :: BM.App GameState e ()
app = BM.App {
    BM.appDraw = draw,
    BM.appChooseCursor = BM.showFirstCursor,
    BM.appHandleEvent = handleEvent,
    BM.appStartEvent = BM.continueWithoutRedraw,
    BM.appAttrMap = const $ A.forceAttrMap VA.defAttr
}

handleEvent :: T.BrickEvent n e -> T.EventM n GameState ()
handleEvent (T.VtyEvent e) = case e of
    -- ESC para salir
    V.EvKey V.KEsc        [] -> BM.halt 
    -- Enter para siguiente turno
    V.EvKey V.KEnter      [] -> do
                                    s <- get
                                    ns <- liftIO $ updateState s
                                    put ns
    _                        -> BM.continueWithoutRedraw

draw :: GameState -> [T.Widget n]
draw state = return (ui state)

internalPaddingX = 5
internalPaddingY = 3
distFromBorder = 10

ui :: GameState -> T.Widget n
ui s = 
    let card1Widget = drawCard (cardPlayer1 s) "Jugador 1" (Just ((player1Points s, usedPlayer1 s)))
        cardCommonWidget = drawCard (cardCommon s) "Carta común" Nothing
        card2Widget = drawCard (cardPlayer2 s) "Jugador 2" (Just ((player2Points s, usedPlayer2 s)))
        cardsWidget = padRight (Pad distFromBorder) card1Widget  
                      <+> vBorder <+> padLeftRight distFromBorder cardCommonWidget
                      <+> vBorder <+> padLeft (Pad distFromBorder) card2Widget
    in
    padAll 2 $
    withBorderStyle unicode $
    borderWithLabel (str "Dobble!") $
    center cardsWidget

drawCard :: Card -> String -> Maybe (Int, Card) -> T.Widget n
drawCard card label maybePlayed =
    let numSymbols = length card
        numRows = ceiling (sqrt (fromIntegral numSymbols))
        symbolsPerRow = ceiling (fromIntegral numSymbols / fromIntegral numRows)
        symbolWidgets = map drawSymbol card
        rows = splitIntoRows symbolsPerRow symbolWidgets
        paddedSymbolRows = map hBox rows
        cardWidget = hCenter $ vBox paddedSymbolRows
        cardWithLabel = borderWithLabel (str label) cardWidget
        -- cardWithPoints = maybe cardWithLabel (\points -> cardWithLabel <=> str (" (" ++ show points ++ " points)")) maybePoints
        cardWithPoints = maybe cardWithLabel (\played -> cardWithLabel <=> displayPlayed played) maybePlayed
        cardWithTopLabel = padBottom (Pad internalPaddingY) cardWithPoints
    in
    padLeftRight internalPaddingX cardWithTopLabel

displayPlayed :: (Int, Card) -> T.Widget n
displayPlayed (points, used) = vBox $ map hCenter [hBorder, str (" (" ++ show points ++ " points)"), hBorder, str used]

splitIntoRows :: Int -> [T.Widget n] -> [[T.Widget n]]
splitIntoRows _ [] = []
splitIntoRows n xs = take n xs : splitIntoRows n (drop n xs)

drawSymbol :: Symbol -> T.Widget n
drawSymbol symbol = str [symbol] <+> str " "

incrementPlayerPoints :: Int -> GameState -> GameState
incrementPlayerPoints playerNum gameState
    | playerNum == 1 = gameState { player1Points = player1Points gameState + 1 }
    | playerNum == 2 = gameState { player2Points = player2Points gameState + 1 }
    | otherwise = gameState
