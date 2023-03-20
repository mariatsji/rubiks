module Main where

import Cube
import Parser

import Brick
import qualified Data.Text.IO as TIO
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
    dummy = ( face
            <+> face
            <+> face
        ) <=> ( face
            <+> face
            <+> face
        ) <=> ( face
            <+> face
            <+> face
        )


face :: Widget ()
face = hLimit 6 (vLimit 3 (fill ' '))

draw :: Face -> Widget ()
draw (a, b, c, d, e, f, g, h, i) =
    let dra po = withAttr (color po) face
     in ( dra a
            <+> dra b
            <+> dra c
        ) <=> ( dra d
            <+> dra e
            <+> dra f
        ) <=> ( dra g
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
    case ev of
        VtyEvent (EvKey (KChar 'r') _) -> do
            let m = Move Torso Clockwise
            cube <- get
            put (move m cube)
        VtyEvent (EvKey (KChar 'e') _) -> do
            let m = Move Torso CounterClockwise
            cube <- get
            put (move m cube)
        VtyEvent (EvKey (KChar 'f') _) -> do
            let m = Move Legs Clockwise
            cube <- get
            put (move m cube)
        VtyEvent (EvKey (KChar 'd') _) -> do
            let m = Move Legs CounterClockwise
            cube <- get
            put (move m cube)
        VtyEvent (EvKey (KChar 'c') _) -> do
            let m = Move Feet Clockwise
            cube <- get
            put (move m cube)
        VtyEvent (EvKey (KChar 'x') _) -> do
            let m = Move Feet CounterClockwise
            cube <- get
            put (move m cube)
        VtyEvent (EvKey (KChar 'h') _) -> do
            let m = Move RightArm Clockwise
            cube <- get
            put (move m cube)
        VtyEvent (EvKey (KChar 'g') _) -> do
            let m = Move RightArm CounterClockwise
            cube <- get
            put (move m cube)
        VtyEvent (EvKey (KChar 's') _) -> do
            let m = Move LeftArm Clockwise
            cube <- get
            put (move m cube)
        VtyEvent (EvKey (KChar 'a') _) -> do
            let m = Move LeftArm CounterClockwise
            cube <- get
            put (move m cube)
        VtyEvent (EvKey (KChar '4') _) -> do
            let m = Move Head Clockwise
            cube <- get
            put (move m cube)
        VtyEvent (EvKey (KChar '3') _) -> do
            let m = Move Head CounterClockwise
            cube <- get
            put (move m cube)
        VtyEvent (EvKey (KChar 'q') _) -> halt
        VtyEvent (EvKey KEsc _) -> put initCube
        _ -> pure ()
    
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
    args <- getArgs
    case args of
        [] ->  void $ defaultMain app initCube
        (file:_) -> do
            t <- TIO.readFile file
            case movesFromText t of
                Left e -> putStrLn e
                Right ms -> void $ defaultMain app (fromMoves ms)
        