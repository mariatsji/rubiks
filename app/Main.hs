{-# language TemplateHaskell #-}
module Main where

import Cube

import Brick
import Brick.AttrMap
import Brick.Widgets.Core ((<=>), (<+>), hBox, vBox)
import Graphics.Vty.Attributes (defAttr)
import Lens.Micro.Mtl
import Lens.Micro.TH
import Relude

-- must be free in n?
data MyAppState n = MyAppState Cube

makeLenses ''MyAppState

drawUi :: MyAppState () -> [Widget ()]
drawUi _ = [ vBox
    [ hBox [ dummy, headWidget, dummy ],
      hBox [ leftWidget, torsoWidget, rightWidget ],
      hBox [ dummy, legsWidget, dummy ],
      hBox [ dummy, feetWidget, dummy ]
    ]]
    where
        fillNine c = hLimit 3 (vLimit 3 ( fill c ))
        dummy = fillNine ' '
        headWidget = fillNine 'R'
        leftWidget = fillNine 'G'
        torsoWidget = fillNine 'Y'
        rightWidget = fillNine 'B'
        legsWidget = fillNine 'O'
        feetWidget = fillNine 'W'

appEvent :: BrickEvent () e -> EventM () (MyAppState ()) ()
appEvent = const $ pure ()

app :: App (MyAppState ()) e ()
app = App {
        appDraw = drawUi,
        appChooseCursor = \_ _ ->  Nothing,
        appHandleEvent = appEvent,
        appStartEvent = pure (),
        appAttrMap = \_ -> attrMap defAttr []
    }

initState :: MyAppState ()
initState = MyAppState initCube

main :: IO ()
main = do
    void $ defaultMain app initState