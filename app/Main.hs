module Main where

import Cube

import Brick
import Relude

ui :: Widget ()
ui = str "Rubiks Cube"

main :: IO ()
main = do
    let cube = initCube
    simpleMain ui
