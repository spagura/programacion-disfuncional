module Dobble.Ui where

import qualified Brick.Main as BM
import qualified Brick.Types as T
import qualified Brick.AttrMap as A
import qualified Graphics.Vty.Attributes as V
import Brick (Widget, (<+>), str, withBorderStyle, joinBorders)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (padAll)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)


data GameState = GameState {
    cards :: [String],
    commonSymbol :: Char
} deriving (Show, Read, Eq, Ord)

initState :: IO GameState
initState = return $ GameState {
    cards = ["1", "2"],
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
    BM.appAttrMap = const $ A.forceAttrMap V.defAttr
}

handleEvent :: T.BrickEvent n e -> T.EventM n GameState ()
handleEvent e = BM.continueWithoutRedraw


draw :: GameState -> [T.Widget n]
draw s = return ui

ui :: T.Widget n
ui = 
    padAll 1 $
    joinBorders $
    withBorderStyle unicode $
    borderWithLabel (str "Dobble!") $
    (center (str "Left") <+> vBorder <+> center (str "Right"))

