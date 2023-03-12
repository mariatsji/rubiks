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
    ( Sticker
    , Sticker
    , Sticker
    , Sticker
    , Sticker
    , Sticker
    , Sticker
    , Sticker
    , Sticker
    )

makeLenses ''Cube

data Direction = Clockwise | CounterClockwise
    deriving stock (Eq)

data Move = Move Place Direction
    deriving stock (Eq)

allSame :: Sticker -> Face
allSame c =
    ( c
    , c
    , c
    , c
    , c
    , c
    , c
    , c
    , c
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

down :: Lens' Face (Sticker, Sticker, Sticker)
down = lens
    (\(_, _, _, _, _, _, a, b, c) -> (a, b, c))
    (\(a,b,c,d,e,f,_,_,_) (x,y,z) -> (a,b,c,d,e,f,x,y,z))

up :: Lens' Face (Sticker, Sticker, Sticker)
up = lens
    (\(a, b, c, _, _, _, _, _, _) -> (a, b, c))
    (\(_,_,_,d,e,f,g,h,i) (x,y,z) -> (x,y,z,d,e,f,g,h,i))

left :: Lens' Face (Sticker, Sticker, Sticker)
left = lens
    (\(a,_,_,b,_,_,c,_,_) -> (a,b,c))
    (\(_,b,c,_,e,f,_,g,h) (x,y,z) -> (x,b,c,y,e,f,z,g,h))

right :: Lens' Face (Sticker, Sticker, Sticker)
right = lens
    (\(_,_,a,_,_,b,_,_,c) -> (a,b,c))
    (\(a,b,_,d,e,_,g,h,_) (x,y,z) -> (a,b,x,d,e,y,g,h,z))

rev :: SimpleGetter (Sticker, Sticker, Sticker) (Sticker, Sticker, Sticker)
rev = to (\(a,b,c) -> (c,b,a))

keep :: SimpleGetter (Sticker, Sticker, Sticker) (Sticker, Sticker, Sticker)
keep = to id

leftOf :: Place -> Len
leftOf = (\(_,_,_,x,_) -> x) . neighbors

rightOf :: Place -> Len
rightOf = (\(_,x,_,_,_) -> x) . neighbors
    
upOf :: Place -> Len
upOf = (\(x,_,_,_,_) -> x) . neighbors

downOf :: Place -> Len
downOf = (\(_,_,x,_,_) -> x) . neighbors

move :: Move -> Cube -> Cube
move (Move place dir) cube =
    let (myUp, myRight, myDown, myLeft, mySelf) = neighbors place
    in case dir of
        Clockwise ->
            let cachedUp = cube ^. myUp . down . keep
            in cube
                & myUp . down .~ ( cube ^. myLeft . right . rev )
                & myLeft . right .~ ( cube ^. myDown . up . keep )
                & myDown . up .~ ( cube ^. myRight . left . rev )
                & myRight . left .~ cachedUp
                & mySelf %~ rotate Clockwise
        CounterClockwise ->
            let cachedUp = cube ^. myUp . down . rev
            in cube
                & myUp . down .~ ( cube ^. myRight . left . keep )
                & myRight . left .~ ( cube ^. myDown . up . rev )
                & myDown . up .~ ( cube ^. myLeft . right . keep )
                & myLeft . right .~ cachedUp
                & mySelf %~ rotate CounterClockwise

     
rotate :: Direction -> Face -> Face
rotate Clockwise (a,b,c, d,e,f, g,h,i) = (g,d,a ,h,e,b ,i,f,c )
rotate CounterClockwise (a,b,c, d,e,f, g,h,i) = (c,f,i , b,e,h , g,d,a )
{--
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

--}