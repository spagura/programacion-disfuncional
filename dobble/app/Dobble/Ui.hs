module Dobble.Ui where

import Brick (Widget, (<+>), str, withBorderStyle, joinBorders)
import qualified Brick.Main as BM
import qualified Brick.Types as T
import qualified Brick.AttrMap as A
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Core (padAll, vBox, hBox, padRight, padLeft, padTopBottom, padLeftRight, padBottom, withAttr, Padding(Pad), (<=>))
import Brick.Widgets.Border (borderWithLabel, vBorder, hBorder)
import Brick.Widgets.Border.Style (unicode)
import System.IO
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes as VA
import System.Random (randomRIO)
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Writer
import Dobble.Utils
import Dobble.Card
import Dobble.Round


data GameState = GameState {
    cardPlayer1 :: Card,
    cardPlayer2 :: Card,
    cardCommon :: Card,
    player1Points :: Int,
    player2Points :: Int,
    usedPlayer1 :: Card,
    usedPlayer2 :: Card,
    maxPoints :: Int,
    gameFinish :: Bool
} deriving (Show, Read, Eq, Ord)

initState :: IO GameState
initState = do
    putStrLn "Ingrese el número máximo de puntos:"
    maxPts <- readLn
    (card1, card2, card3) <- generateCards 9
    let (used1, used2) = playRound card1 card2 card3
    return $ GameState {
        cardPlayer1 = card1,
        cardPlayer2 = card2,
        cardCommon = card3,
        player1Points = 0,
        player2Points = 0,
        usedPlayer1 = used1,
        usedPlayer2 = used2,
        maxPoints = maxPts,
        gameFinish = False
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
    (card1, card2, card3) <- generateCards 9
    let (used1, used2) = playRound card1 card2 card3
    let updatedGameState = incrementPlayerPoints (findWinner used1 used2) s
    let finish = if player1Points s >= maxPoints s || player2Points s >= maxPoints s then True else False
    return (s { 
            usedPlayer1 = used1, 
            usedPlayer2 = used2, 
            cardPlayer1 = card1, 
            cardPlayer2 = card2, 
            cardCommon = card3, 
            player1Points = player1Points updatedGameState, 
            player2Points = player2Points updatedGameState,
            gameFinish = finish
        })

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
    BM.appAttrMap = const $ aMap
}

aMap :: A.AttrMap
aMap = A.attrMap V.defAttr
    [ (A.attrName "symbolA", VA.withForeColor VA.defAttr (V.rgbColor 40 0 50))
    , (A.attrName "symbolB", VA.withForeColor VA.defAttr (V.rgbColor 70 0 180))
    , (A.attrName "symbolC", VA.withForeColor VA.defAttr (V.rgbColor 100 0 255))
    , (A.attrName "symbolD", VA.withForeColor VA.defAttr (V.rgbColor 130 50 0))
    , (A.attrName "symbolE", VA.withForeColor VA.defAttr (V.rgbColor 160 180 0))
    , (A.attrName "symbolF", VA.withForeColor VA.defAttr (V.rgbColor 190 255 0))
    , (A.attrName "symbolG", VA.withForeColor VA.defAttr (V.rgbColor 220 0 80))
    , (A.attrName "symbolH", VA.withForeColor VA.defAttr (V.rgbColor 255 0 0))
    , (A.attrName "symbolI", VA.withForeColor VA.defAttr (V.rgbColor 50 40 0))
    , (A.attrName "symbolJ", VA.withForeColor VA.defAttr (V.rgbColor 180 70 0))
    , (A.attrName "symbolK", VA.withForeColor VA.defAttr (V.rgbColor 255 100 0))
    , (A.attrName "symbolL", VA.withForeColor VA.defAttr (V.rgbColor 0 130 50))
    , (A.attrName "symbolM", VA.withForeColor VA.defAttr (V.rgbColor 0 160 180))
    , (A.attrName "symbolN", VA.withForeColor VA.defAttr (V.rgbColor 0 190 255))
    , (A.attrName "symbolO", VA.withForeColor VA.defAttr (V.rgbColor 80 220 0))
    , (A.attrName "symbolP", VA.withForeColor VA.defAttr (V.rgbColor 0 255 0))
    , (A.attrName "symbolQ", VA.withForeColor VA.defAttr (V.rgbColor 50 0 40))
    , (A.attrName "symbolR", VA.withForeColor VA.defAttr (V.rgbColor 180 0 70))
    , (A.attrName "symbolS", VA.withForeColor VA.defAttr (V.rgbColor 255 0 100))
    , (A.attrName "symbolT", VA.withForeColor VA.defAttr (V.rgbColor 0 50 130))
    , (A.attrName "symbolU", VA.withForeColor VA.defAttr (V.rgbColor 0 180 160))
    , (A.attrName "symbolV", VA.withForeColor VA.defAttr (V.rgbColor 0 255 190))
    , (A.attrName "symbolW", VA.withForeColor VA.defAttr (V.rgbColor 80 0 220))
    , (A.attrName "symbolX", VA.withForeColor VA.defAttr (V.rgbColor 0 0 255))
    , (A.attrName "symbolY", VA.withForeColor VA.defAttr (V.rgbColor 255 255 0))
    , (A.attrName "symbolZ", VA.withForeColor VA.defAttr (V.rgbColor 0 255 255))
    , (A.attrName "symbol", VA.withForeColor VA.defAttr V.white) ]

handleEvent :: T.BrickEvent n e -> T.EventM n GameState ()
handleEvent (T.VtyEvent e) = case e of
    -- ESC para salir
    V.EvKey V.KEsc        [] -> BM.halt 
    -- Enter para siguiente turno
    V.EvKey V.KEnter      [] -> do
                                    s <- get
                                    ns <- liftIO $ updateState s
                                    put ns
    -- 'g' para guardar el estado actual del juego                                
    V.EvKey (V.KChar 'g') [] -> do
        s <- get
        liftIO $ printToTextFile (show s) "estado.txt"

    _                        -> BM.continueWithoutRedraw

draw :: GameState -> [T.Widget n]
draw state = 
    if gameFinish state
    then 
        if player1Points state >= maxPoints state 
        then return (
            joinBorders $
            withBorderStyle unicode $
            borderWithLabel (str "Dobble!") $
            (center (str "Ganador jugador 1")))
        else 
            return (
            joinBorders $
            withBorderStyle unicode $
            borderWithLabel (str "Dobble!") $
            (center (str "Ganador jugador 2")))
    else return (ui state)

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
        cardWithPoints = maybe cardWithLabel (\played -> cardWithLabel <=> displayPlayed played) maybePlayed
        cardWithTopLabel = padBottom (Pad internalPaddingY) cardWithPoints
    in
    padLeftRight internalPaddingX cardWithTopLabel

displayPlayed :: (Int, Card) -> T.Widget n
displayPlayed (points, used) = vBox $ map hCenter [hBorder, str (" (" ++ show points ++ " points)"), hBorder, hBox (map drawSymbol used)]

splitIntoRows :: Int -> [T.Widget n] -> [[T.Widget n]]
splitIntoRows _ [] = []
splitIntoRows n xs = take n xs : splitIntoRows n (drop n xs)

drawSymbol :: Symbol -> T.Widget n
drawSymbol symbol 
    | symbol == 'A' = drawSymbol_ "symbolA" symbol
    | symbol == 'B' = drawSymbol_ "symbolB" symbol
    | symbol == 'C' = drawSymbol_ "symbolC" symbol
    | symbol == 'D' = drawSymbol_ "symbolD" symbol
    | symbol == 'E' = drawSymbol_ "symbolE" symbol
    | symbol == 'F' = drawSymbol_ "symbolF" symbol
    | symbol == 'G' = drawSymbol_ "symbolG" symbol
    | symbol == 'H' = drawSymbol_ "symbolH" symbol
    | symbol == 'I' = drawSymbol_ "symbolI" symbol
    | symbol == 'J' = drawSymbol_ "symbolJ" symbol
    | symbol == 'K' = drawSymbol_ "symbolK" symbol
    | symbol == 'L' = drawSymbol_ "symbolL" symbol
    | symbol == 'M' = drawSymbol_ "symbolM" symbol
    | symbol == 'N' = drawSymbol_ "symbolN" symbol
    | symbol == 'O' = drawSymbol_ "symbolO" symbol
    | symbol == 'P' = drawSymbol_ "symbolP" symbol
    | symbol == 'Q' = drawSymbol_ "symbolQ" symbol
    | symbol == 'R' = drawSymbol_ "symbolR" symbol
    | symbol == 'S' = drawSymbol_ "symbolS" symbol
    | symbol == 'T' = drawSymbol_ "symbolT" symbol
    | symbol == 'U' = drawSymbol_ "symbolU" symbol
    | symbol == 'V' = drawSymbol_ "symbolV" symbol
    | symbol == 'W' = drawSymbol_ "symbolW" symbol
    | symbol == 'X' = drawSymbol_ "symbolX" symbol
    | symbol == 'Y' = drawSymbol_ "symbolY" symbol
    | symbol == 'Z' = drawSymbol_ "symbolZ" symbol
    | otherwise = drawSymbol_ "symbol" symbol

drawSymbol_ :: String -> Symbol -> T.Widget n
drawSymbol_ a symbol =
    withAttr (A.attrName a) $ str [symbol] <+> str " "

incrementPlayerPoints :: Int -> GameState -> GameState
incrementPlayerPoints playerNum gameState
    | playerNum == 1 && player1Points gameState < maxPoints gameState = gameState { player1Points = player1Points gameState + 1 }
    | playerNum == 2 && player2Points gameState < maxPoints gameState = gameState { player2Points = player2Points gameState + 1 }
    | otherwise = gameState
