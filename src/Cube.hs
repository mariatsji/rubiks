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

type Len = Lens' Cube Face

-- up, right, down, left, myself
-- not enough! Also what "stripe" do I set, and what "stripe" do I steal from neighbor! This is one single lens!
neighbors :: Place -> (Len, Len, Len, Len, Len)
neighbors = \case
    Torso -> (head, rightArm, legs, leftArm, torso)
    Head -> (feet, rightArm, torso, leftArm, head)
    RightArm -> (head, feet, legs, torso, rightArm)
    LeftArm -> (head, torso, legs, feet, leftArm)
    Legs -> (torso, rightArm, feet, leftArm, feet)
    Feet -> (legs, rightArm, head, leftArm, feet)

move :: Move -> Cube -> Cube
move (Move place dir) cube = 
    let (myUp, myRight, myDown, myLeft, mySelf) = neighbors place
    in case dir of
        Clockwise ->
                cube
                    & myUp . _3 . _1 .~ (cube ^. myLeft . _3 . _3)
                    & myUp . _3 . _2 .~ (cube ^. myLeft . _2 . _3)
                    & myUp . _3 . _3 .~ (cube ^. myLeft . _1 . _3)
                    & myLeft . _1 . _3 .~ (cube ^. myDown . _1 . _1)
                    & myLeft . _2 . _3 .~ (cube ^. myDown . _1 . _2)
                    & myLeft . _3 . _3 .~ (cube ^. myDown . _1 . _3)
                    & myRight . _1 . _1 .~ (cube ^. myUp . _3 . _1)
                    & myRight . _2 . _1 .~ (cube ^. myUp . _3 . _2)
                    & myRight . _3 . _1 .~ (cube ^. myUp . _3 . _3)
                    & myDown . _1 . _1 .~ (cube ^. myRight . _1 . _1)
                    & myDown . _1 . _2 .~ (cube ^. myRight . _2 . _1)
                    & myDown . _1 . _3 .~ (cube ^. myRight . _3 . _1)
                    & mySelf %~ rotate dir
        CounterClockwise ->
            cube 
                & myUp . _3 . _1 .~ (cube ^. myRight . _1 . _1)
                & myUp . _3 . _2 .~ (cube ^. myRight . _2 . _1)
                & myUp . _3 . _3 .~ (cube ^. myRight . _3 . _1)
                & myLeft . _3 . _3 .~ (cube ^. myUp . _3 . _1)
                & myLeft . _2 . _3 .~ (cube ^. myUp . _3 . _2)
                & myLeft . _1 . _3 .~ (cube ^. myUp . _3 . _3)
                & myRight . _1 . _1 .~ (cube ^. myDown . _1 . _3)
                & myRight . _2 . _1 .~ (cube ^. myDown . _1 . _2)
                & myRight . _3 . _1 .~ (cube ^. myDown . _1 . _1)
                & myDown . _1 . _1 .~ (cube ^. myLeft . _1 . _3)
                & myDown . _1 . _2 .~ (cube ^. myLeft . _2 . _3)
                & myDown . _1 . _3 .~ (cube ^. myLeft . _3 . _3)
                & mySelf %~ rotate Clockwise
    

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