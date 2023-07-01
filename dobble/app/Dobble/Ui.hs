module Dobble.Ui where

import qualified System.Random as R (next, newStdGen, randomRs, mkStdGen)
import qualified Brick.Main as BM
import qualified Brick.Types as T
import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes as VA
import Brick (Widget, (<+>), str, withBorderStyle, joinBorders)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (padAll, vBox, hBox, padRight, padLeft, padTopBottom, padLeftRight, Padding(Pad))
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Data.List (intercalate, genericReplicate)
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)

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
-- Devuelve una tupla con dos cartas que tienen solamente un simbolo en comun y ese simbolo (en un IO)
generateCardPair :: Int -> IO (Card, Card, Symbol)
generateCardPair n = do
    dup:others <- getNSymbols n
    (firstHalf, secondHalf) <- return $ splitAt (n - 1) others
    card1 <- shuffle $ dup:firstHalf
    card2 <- shuffle $ dup:secondHalf
    return (card1, card2, dup)


-- https://unicodemap.org/
getNSymbols :: Int -> IO [Symbol]
getNSymbols n = do
    shuffledEmojis <- shuffle emojis
    return (take (n * 2 - 1) shuffledEmojis)
  where
    --emojis = ['😈', '👀', '🤡', '😍', '🥲', '🤣', '😀', '👾', '🧲', '🎃']
    --emojis = ['\128512'..'\128591']
    --emojis = [toEnum 0x2660 .. toEnum 0x26FF]
    emojis = ['A' .. 'M']

-- Chquea que dos cartas tengan 1 simbolo en comun
symbolsInCommon :: Card -> Card -> Bool
symbolsInCommon card1 card2 = length (filter (`elem` card2) card1) == 1

-- Función que devuelve True si todas las cartas en el conjunto tienen exactamente una letra en común con todas las demás cartas
checkDeck :: Deck -> Bool
checkDeck deck = all (\card -> all (symbolsInCommon card) (filter (/= card) deck)) deck


data GameState = GameState {
    cardPlayer1 :: Card,
    cardPlayer2 :: Card,
    commonSymbol :: Symbol
} deriving (Show, Read, Eq, Ord)

initState :: IO GameState
initState = do
    (card1, card2, commonSymbol) <- generateCardPair 7
    return $ GameState {
        cardPlayer1 = card1,
        cardPlayer2 = card2,
        commonSymbol = commonSymbol
    }

updateState :: GameState -> IO GameState
updateState s = initState

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
    let cardWidget1 = drawCard (cardPlayer1 s) "Jugador 1"
        cardWidget2 = drawCard (cardPlayer2 s) "Jugador 2"
        cardsWidget = padRight (Pad distFromBorder) cardWidget1  <+> vBorder <+> padLeft (Pad distFromBorder) cardWidget2
    in
    padAll 2 $
    withBorderStyle unicode $
    borderWithLabel (str "Dobble!") $
    center cardsWidget

drawCard :: Card -> String -> T.Widget n
drawCard card label=
    let numSymbols = length card
        numRows = ceiling (sqrt (fromIntegral numSymbols))
        symbolsPerRow = ceiling (fromIntegral numSymbols / fromIntegral numRows)
        symbolWidgets = map drawSymbol card
        rows = splitIntoRows symbolsPerRow symbolWidgets
        paddedSymbolRows = map hBox rows
    in
    borderWithLabel (str label) $ padTopBottom internalPaddingY $ padLeftRight internalPaddingX $ vBox paddedSymbolRows

splitIntoRows :: Int -> [T.Widget n] -> [[T.Widget n]]
splitIntoRows _ [] = []
splitIntoRows n xs = take n xs : splitIntoRows n (drop n xs)

drawSymbol :: Symbol -> T.Widget n
drawSymbol symbol = str([symbol]) <+> str " "

