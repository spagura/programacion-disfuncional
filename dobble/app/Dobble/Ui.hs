module Dobble.Ui where

import qualified System.Random as R (next, newStdGen, randomRs, mkStdGen)
import qualified Brick.Main as BM
import qualified Brick.Types as T
import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes as VA
import Brick (Widget, (<+>), str, withBorderStyle, joinBorders)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (padAll, vBox)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Data.List (intercalate, genericReplicate)
import System.Random (randomRIO)
import Control.Monad (replicateM)

type Symbol = Char
type Card = [Symbol]
type Deck = [Card]

-- symbols = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M']

-- Funcion para hacerle shuffle a una lista
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle items = do
    index <- randomRIO (0, length items - 1)
    rest <- shuffle (take index items ++ drop (index + 1) items)
    return (items !! index : rest)

-- Genera carta al azar con 4 emojis
generateCard :: IO Card
generateCard = do
    shuffledEmojis <- shuffle emojis
    return (take 4 shuffledEmojis)
  where
    emojis = ['😈', '👀', '🤡', '😍', '🥲', '🤣', '😀', '👾', '🧲', '🎃']

-- Genera un deck de 169 cartas
generateDeck :: IO Deck
generateDeck = replicateM 169 generateCard


-- Chquea que dos cartas tengan 1 simbolo en comun
symbolsInCommon :: Card -> Card -> Bool
symbolsInCommon card1 card2 = length (filter (`elem` card2) card1) == 1

-- Función que devuelve True si todas las cartas en el conjunto tienen exactamente una letra en común con todas las demás cartas
checkDeck :: Deck -> Bool
checkDeck deck = all (\card -> all (symbolsInCommon card) (filter (/= card) deck)) deck


data GameState = GameState {
    cards :: Deck,
    commonSymbol :: Symbol,
    bobo :: [Char]
} deriving (Show, Read, Eq, Ord)

initState :: IO GameState
initState = do
    deck <- generateDeck
    return $ GameState
        { cards = deck
        , bobo = ['1', '2', '3', '4']
        , commonSymbol = '1'
        }

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

-- ESC para salir
handleEvent (T.VtyEvent e) = case e of
    V.EvKey V.KEsc        [] -> BM.halt 
    _                        -> BM.continueWithoutRedraw

draw :: GameState -> [T.Widget n]
draw state = return (ui state)

ui :: GameState -> T.Widget n
ui gameState =
    padAll 1 $
    joinBorders $
    withBorderStyle unicode $
    borderWithLabel (str "Dobble!") $
    vBox $ map cardWidget (cards gameState)
    where
        cardWidget :: String -> Widget n
        cardWidget symbols = center (str symbols) <+> vBorder