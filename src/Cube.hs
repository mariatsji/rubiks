{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Cube (Sticker (..), Move (..), Direction (..), Place (..), Face, Cube (..), initCube, move) where

import Lens.Micro
import Lens.Micro.TH
import Relude hiding (head)

data Sticker = Yellow | Red | Green | White | Orange | Blue
    deriving stock (Eq)

data Place = Head | Torso | LeftArm | RightArm | Legs | Feet
    deriving stock (Eq)

data Cube = Cube
    { _head :: Face
    , _torso :: Face
    , _rightArm :: Face
    , _leftArm :: Face
    , _legs :: Face
    , _feet :: Face
    }
    deriving stock (Eq)

type Face =
    ( (Sticker, Sticker, Sticker)
    , (Sticker, Sticker, Sticker)
    , (Sticker, Sticker, Sticker)
    )

makeLenses ''Cube

data Direction = Clockwise | CounterClockwise
    deriving stock (Eq)

data Move = Move Place Direction
    deriving stock (Eq)

allSame :: Sticker -> Face
allSame c =
    ( (c, c, c)
    , (c, c, c)
    , (c, c, c)
    )

initCube :: Cube
initCube =
    Cube
        { _head = allSame Red
        , _torso = allSame Yellow
        , _rightArm = allSame Blue
        , _leftArm = allSame Green
        , _legs = allSame Orange
        , _feet = allSame White
        }

move :: Move -> Cube -> Cube
move (Move place dir) cube = case (place, dir) of
    (Torso, Clockwise) ->
        cube
            & head . _3 . _1 .~ (cube ^. leftArm . _3 . _3)
            & head . _3 . _2 .~ (cube ^. leftArm . _2 . _3)
            & head . _3 . _3 .~ (cube ^. leftArm . _1 . _3)
            & leftArm . _1 . _3 .~ (cube ^. legs . _1 . _1)
            & leftArm . _2 . _3 .~ (cube ^. legs . _1 . _2)
            & leftArm . _3 . _3 .~ (cube ^. legs . _1 . _3)
            & rightArm . _1 . _1 .~ (cube ^. head . _3 . _1)
            & rightArm . _2 . _1 .~ (cube ^. head . _3 . _2)
            & rightArm . _3 . _1 .~ (cube ^. head . _3 . _3)
            & legs . _1 . _1 .~ (cube ^. rightArm . _1 . _1)
            & legs . _1 . _2 .~ (cube ^. rightArm . _2 . _1)
            & legs . _1 . _3 .~ (cube ^. rightArm . _3 . _1)
            & torso %~ rotate Clockwise
    _ -> cube

rotate :: Direction -> Face -> Face
rotate Clockwise face =
    face
        & _1 . _1 .~ (face ^. _3 . _1)
        & _1 . _2 .~ (face ^. _2 . _1)
        & _1 . _3 .~ (face ^. _1 . _1)
        & _2 . _1 .~ (face ^. _3 . _2)
        & _2 . _2 .~ (face ^. _2 . _2)
        & _2 . _3 .~ (face ^. _1 . _2)
        & _3 . _1 .~ (face ^. _3 . _3)
        & _3 . _2 .~ (face ^. _2 . _3)
        & _3 . _3 .~ (face ^. _1 . _3)
rotate CounterClockwise face =
    face
        & _1 . _1 .~ (face ^. _1 . _3)
        & _1 . _2 .~ (face ^. _2 . _3)
        & _1 . _3 .~ (face ^. _3 . _3)
        & _2 . _1 .~ (face ^. _1 . _2)
        & _2 . _2 .~ (face ^. _2 . _2)
        & _2 . _3 .~ (face ^. _3 . _2)
        & _3 . _1 .~ (face ^. _1 . _1)
        & _3 . _2 .~ (face ^. _1 . _2)
        & _3 . _3 .~ (face ^. _1 . _3)