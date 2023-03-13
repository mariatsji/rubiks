{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Cube (Sticker (..), Move (..), Direction (..), Place (..), Face, Cube (..), initCube, move) where

import Lens.Micro.Platform
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
down =
    lens
        (\(_, _, _, _, _, _, a, b, c) -> (a, b, c))
        (\(a, b, c, d, e, f, _, _, _) (x, y, z) -> (a, b, c, d, e, f, x, y, z))

up :: Lens' Face (Sticker, Sticker, Sticker)
up =
    lens
        (\(a, b, c, _, _, _, _, _, _) -> (a, b, c))
        (\(_, _, _, d, e, f, g, h, i) (x, y, z) -> (x, y, z, d, e, f, g, h, i))

left :: Lens' Face (Sticker, Sticker, Sticker)
left =
    lens
        (\(a, _, _, b, _, _, c, _, _) -> (a, b, c))
        (\(_, b, c, _, e, f, _, g, h) (x, y, z) -> (x, b, c, y, e, f, z, g, h))

right :: Lens' Face (Sticker, Sticker, Sticker)
right =
    lens
        (\(_, _, a, _, _, b, _, _, c) -> (a, b, c))
        (\(a, b, _, d, e, _, g, h, _) (x, y, z) -> (a, b, x, d, e, y, g, h, z))

rev :: SimpleGetter (Sticker, Sticker, Sticker) (Sticker, Sticker, Sticker)
rev = to (\(a, b, c) -> (c, b, a))

keep :: SimpleGetter (Sticker, Sticker, Sticker) (Sticker, Sticker, Sticker)
keep = to id

leftOf :: Place -> Len
leftOf = (\(_, _, _, x, _) -> x) . neighbors

rightOf :: Place -> Len
rightOf = (\(_, x, _, _, _) -> x) . neighbors

upOf :: Place -> Len
upOf = (\(x, _, _, _, _) -> x) . neighbors

downOf :: Place -> Len
downOf = (\(_, _, x, _, _) -> x) . neighbors

move :: Move -> Cube -> Cube
move (Move place dir) cube =
    case place of
        Torso ->
            case dir of
                Clockwise ->
                    cube
                        & head . down .~ (cube ^. leftArm . right . rev)
                        & leftArm . right .~ (cube ^. legs . up . keep)
                        & legs . up .~ (cube ^. rightArm . left . rev)
                        & rightArm . left .~ (cube ^. head . down . keep)
                        & torso %~ rotate Clockwise
                CounterClockwise ->
                    cube
                        & head . down .~ (cube ^. rightArm . left . keep)
                        & rightArm . left .~ (cube ^. legs . up . rev)
                        & legs . up .~ (cube ^. leftArm . right . keep)
                        & leftArm . right .~ (cube ^. head . down . rev)
                        & torso %~ rotate CounterClockwise
        Head ->
            case dir of
                Clockwise ->
                    cube
                        & feet . down .~ (cube ^. leftArm . up . rev)
                        & leftArm . up .~ (cube ^. torso . up . keep)
                        & torso . up .~ (cube ^. rightArm . up . keep)
                        & rightArm . up .~ (cube ^. feet . down . rev)
                        & head %~ rotate Clockwise
                CounterClockwise ->
                    cube
                        & feet . down .~ (cube ^. rightArm . up . keep)
                        & rightArm . up .~ (cube ^. torso . up . rev)
                        & torso . up .~ (cube ^. leftArm . up . keep)
                        & leftArm . up .~ (cube ^. feet . down . rev)
                        & head %~ rotate CounterClockwise
        Legs ->
            case dir of
                Clockwise ->
                    cube
                        & torso . down .~ (cube ^. leftArm . down . rev)
                        & rightArm . down .~ (cube ^. torso . down . keep)
                        & feet . up .~ (cube ^. rightArm . down . rev)
                        & leftArm . down .~ (cube ^. feet . up . keep)
                        & legs %~ rotate Clockwise
                CounterClockwise ->
                    cube
                        & torso . down .~ (cube ^. rightArm . down . keep)
                        & rightArm . down .~ (cube ^. feet . up . rev)
                        & feet . up .~ (cube ^. leftArm . down . keep)
                        & leftArm . down .~ (cube ^. torso . down . rev)
                        & legs %~ rotate CounterClockwise
        _ -> cube

rotate :: Direction -> Face -> Face
rotate Clockwise (a, b, c, d, e, f, g, h, i) = (g, d, a, h, e, b, i, f, c)
rotate CounterClockwise (a, b, c, d, e, f, g, h, i) = (c, f, i, b, e, h, g, d, a)