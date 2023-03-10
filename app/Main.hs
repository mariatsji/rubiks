module Main where

import Cube

import Brick
import Brick.Widgets.Core ((<=>), (<+>), hBox, vBox)
import Relude

ui :: Widget ()
ui = vBox
    [ hBox [ dummy, headWidget, dummy ],
      hBox [ leftWidget, torsoWidget, rightWidget ],
      hBox [ dummy, legsWidget, dummy ],
      hBox [ dummy, feetWidget, dummy ]
    ] -- str "Rubiks Cube"
    where
        fillNine c = hLimit 3 (vLimit 3 ( fill c ))
        dummy = fillNine ' '
        headWidget = fillNine 'R'
        leftWidget = fillNine 'G'
        torsoWidget = fillNine 'Y'
        rightWidget = fillNine 'B'
        legsWidget = fillNine 'O'
        feetWidget = fillNine 'W'
        

main :: IO ()
main = do
    let cube = initCube
    simpleMain ui
