module Dobble.Ui where

import qualified System.Random as R (next, newStdGen, randomRs, mkStdGen)
import qualified Brick.Main as BM
import qualified Brick.Types as T
import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes as VA
import Brick (Widget, (<+>), str, withBorderStyle, joinBorders)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (padAll)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)

type Symbol = Char
type Card = [Symbol]
type Deck = [Card]

-- symbols = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M']

-- Genera carta al azar con 4 letras desde la A hasta la M
generateCard :: Card
generateCard =  do
    let gen = R.mkStdGen 10
    take 4 $ R.randomRs ('A', 'M') $ gen
    

generateDeck :: Deck
generateDeck = do
    sequence $ replicate 169 $ generateCard


-- Chquea que dos cartas tengan 1 simbolo en comun
symbolsInCommon :: Card -> Card -> Bool
symbolsInCommon card1 card2 = length (filter (`elem` card2) card1) == 1

-- Función que devuelve True si todas las cartas en el conjunto tienen exactamente una letra en común con todas las demás cartas
checkDeck :: Deck -> Bool
checkDeck deck = all (\card -> all (symbolsInCommon card) (filter (/= card) deck)) deck


data GameState = GameState {
    cards :: Deck,
    commonSymbol :: Symbol
} deriving (Show, Read, Eq, Ord)

initState :: IO GameState
initState = return $ GameState {
    cards = generateDeck,
    commonSymbol = '1'
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
draw s = return ui

ui :: T.Widget n
ui = 
    padAll 1 $
    joinBorders $
    withBorderStyle unicode $
    borderWithLabel (str "Dobble!") $
    (center (str "Left") <+> vBorder <+> center (str "Right"))
