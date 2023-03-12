module Main where

import Cube

import Brick
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Attributes.Color (
    blue,
    green,
    red,
    rgbColor,
    white,
    yellow,
 )
import Graphics.Vty.Input.Events
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
    dummy = hLimit 6 (vLimit 3 (fill ' '))

draw :: Face -> Widget ()
draw (a, b, c, d, e, f, g, h, i) =
    let dra po = withAttr (color po) (str "  ")
     in ( dra a
            <+> dra b
            <+> dra c
        )
            <=> ( dra d
                    <+> dra e
                    <+> dra f
                )
            <=> ( dra g
                    <+> dra h
                    <+> dra i
                )
  where
    color :: Sticker -> AttrName
    color =
        attrName . \case
            Red -> "red"
            Green -> "green"
            Blue -> "blue"
            Yellow -> "yellow"
            White -> "white"
            Orange -> "orange"

appEvent :: BrickEvent () e -> EventM () Cube ()
appEvent ev = 
    let mov = case ev of
            VtyEvent (EvKey (KChar 'r') _) -> Just $ Move Torso Clockwise
            VtyEvent (EvKey (KChar 'e') _) -> Just $ Move Torso CounterClockwise
            VtyEvent (EvKey (KChar 'f') _) -> Just $ Move Legs Clockwise
            VtyEvent (EvKey (KChar 'd') _) -> Just $ Move Legs CounterClockwise
            VtyEvent (EvKey (KChar 'c') _) -> Just $ Move Feet Clockwise
            VtyEvent (EvKey (KChar 'x') _) -> Just $ Move Feet CounterClockwise
            VtyEvent (EvKey (KChar 'h') _) -> Just $ Move RightArm Clockwise
            VtyEvent (EvKey (KChar 'g') _) -> Just $ Move RightArm CounterClockwise
            VtyEvent (EvKey (KChar 's') _) -> Just $ Move LeftArm Clockwise
            VtyEvent (EvKey (KChar 'a') _) -> Just $ Move LeftArm CounterClockwise
            VtyEvent (EvKey (KChar '4') _) -> Just $ Move Head Clockwise
            VtyEvent (EvKey (KChar '3') _) -> Just $ Move Head CounterClockwise
            _ -> Nothing
    in maybe
            halt
            (\m -> do
                cube <- get
                put (move m cube)
            )
            mov
    
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
                , (attrName "orange", bg $ rgbColor @Int 255 165 0)
                ]
        }

main :: IO ()
main = do
    void $ defaultMain app initCube