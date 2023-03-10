module Main where

import Cube

import Brick
import Graphics.Vty (Attr)
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Attributes.Color (
    blue,
    green,
    red,
    rgbColor,
    white,
    yellow,
 )
import Relude

drawUi :: Cube -> [Widget ()]
drawUi cube =
    [ vBox
        [ hBox [dummy, draw $ _head cube, dummy]
        , hBox [draw $ _leftArm cube, draw $ _torso cube, draw $ _rightArm cube]
        , hBox [dummy, draw $ _legs cube, dummy]
        , hBox [dummy, draw $ _feet cube, dummy]
        ]
    ]
  where
    fillNine c = hLimit 3 (vLimit 3 (fill c))
    dummy = fillNine ' '
    headWidget = fillNine 'R'
    leftWidget = fillNine 'G'
    torsoWidget = fillNine 'Y'
    rightWidget = fillNine 'B'
    legsWidget = fillNine 'O'
    feetWidget = fillNine 'W'

draw :: Face -> Widget ()
draw ((a, b, c), (d, e, f), (g, h, i)) =
    ( withAttr (color a) (str " ")
        <+> withAttr (color b) (str " ")
        <+> withAttr (color c) (str " ")
    )
        <=> ( withAttr (color d) (str " ")
                <+> withAttr (color e) (str " ")
                <+> withAttr (color f) (str " ")
            )
        <=> ( withAttr (color g) (str " ")
                <+> withAttr (color h) (str " ")
                <+> withAttr (color i) (str " ")
            )
  where
    color :: Sticker -> AttrName
    color = attrName . \case
        Red -> "red"
        Green -> "green"
        Blue -> "blue"
        Yellow -> "yellow"
        White -> "white"
        Orange -> "orange"

appEvent :: BrickEvent () e -> EventM () Cube ()
appEvent = const halt

app :: App Cube e ()
app =
    App
        { appDraw = drawUi
        , appChooseCursor = \_ _ -> Nothing
        , appHandleEvent = appEvent
        , appStartEvent = pure ()
        , appAttrMap = \_ ->
            attrMap
                defAttr
                [ (attrName "blue", bg blue)
                , (attrName "red", bg red)
                , (attrName "green", bg green)
                , (attrName "white", bg white)
                , (attrName "yellow", bg yellow)
                , (attrName "orange", bg $ rgbColor 255 165 0)
                ]
        }

main :: IO ()
main = do
    void $ defaultMain app initCube