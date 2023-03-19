{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Cube (Sticker (..), Move (..), Direction (..), Place (..), Face, Cube (..), initCube, fromMoves, move) where

import Lens.Micro.Platform
import Relude hiding (head)
import qualified Text.Show

data Sticker = Yellow | Red | Green | White | Orange | Blue
    deriving stock (Eq)

data Place = Head | Torso | LeftArm | RightArm | Legs | Feet
    deriving stock (Eq)

instance Show Place where
    show Torso = "F"
    show RightArm = "R"
    show LeftArm = "L"
    show Head = "U"
    show Feet = "B"
    show Legs = "D"


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

instance Show Direction where
    show Clockwise = ""
    show CounterClockwise = "'"

data Move = Move Place Direction
    deriving stock (Eq)

instance Show Move where
    show (Move place dir) = show place <> show dir

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
        { _head = allSame White
        , _torso = allSame Green
        , _rightArm = allSame Red
        , _leftArm = allSame Orange
        , _legs = allSame Yellow
        , _feet = allSame Blue
        }

fromMoves :: [Move] -> Cube
fromMoves = foldl' (flip move) initCube

type Len = Lens' Cube Face

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
                CounterClockwise ->
                    cube
                        & head . down .~ (cube ^. rightArm . left . keep)
                        & rightArm . left .~ (cube ^. legs . up . rev)
                        & legs . up .~ (cube ^. leftArm . right . keep)
                        & leftArm . right .~ (cube ^. head . down . rev)
            & torso %~ rotate dir
        Head ->
            case dir of
                Clockwise ->
                    cube
                        & feet . down .~ (cube ^. leftArm . up . rev)
                        & leftArm . up .~ (cube ^. torso . up . keep)
                        & torso . up .~ (cube ^. rightArm . up . keep)
                        & rightArm . up .~ (cube ^. feet . down . keep)
                CounterClockwise ->
                    cube
                        & feet . down .~ (cube ^. rightArm . up . keep)
                        & rightArm . up .~ (cube ^. torso . up . keep)
                        & torso . up .~ (cube ^. leftArm . up . keep)
                        & leftArm . up .~ (cube ^. feet . down . rev)
            & head %~ rotate dir
        Legs ->
            case dir of
                Clockwise ->
                    cube
                        & torso . down .~ (cube ^. leftArm . down . keep)
                        & rightArm . down .~ (cube ^. torso . down . keep)
                        & feet . up .~ (cube ^. rightArm . down . rev)
                        & leftArm . down .~ (cube ^. feet . up . keep)
                CounterClockwise ->
                    cube
                        & torso . down .~ (cube ^. rightArm . down . keep)
                        & rightArm . down .~ (cube ^. feet . up . rev)
                        & feet . up .~ (cube ^. leftArm . down . rev)
                        & leftArm . down .~ (cube ^. torso . down . keep)
            & legs %~ rotate dir
        Feet ->
            case dir of
                Clockwise ->
                    cube
                        & legs . down .~ (cube ^. leftArm . left . keep)
                        & leftArm . left .~ (cube ^. head . up . rev)
                        & head . up .~ (cube ^. rightArm . right . rev)
                        & rightArm . right .~ (cube ^. legs . down . keep)
                CounterClockwise ->
                    cube
                        & legs . down .~ (cube ^. rightArm . right . rev)
                        & leftArm . left .~ (cube ^. legs . down . rev)
                        & head . up .~ (cube ^. leftArm . left . keep)
                        & rightArm . right .~ (cube ^. head . up . keep)
            & feet %~ rotate dir
        RightArm ->
            case dir of
                Clockwise ->
                    cube
                        & head . right .~ (cube ^. feet . right . rev)
                        & torso . right .~ (cube ^. head . right . keep)
                        & legs . right .~ (cube ^. torso . right . keep)
                        & feet . right .~ (cube ^. legs . right . rev)
                CounterClockwise ->
                    cube
                        & head . right .~ (cube ^. torso . right . keep)
                        & torso . right .~ (cube ^. legs . right . keep)
                        & legs . right .~ (cube ^. feet . right . rev)
                        & feet . right .~ (cube ^. head . right . rev)
            & rightArm %~ rotate dir
        LeftArm ->
            case dir of
                Clockwise ->
                        cube
                        & head . left .~ (cube ^. feet . left . rev)
                        & torso . left .~ (cube ^. head . left . keep)
                        & legs . left .~ (cube ^. torso . left . keep)
                        & feet . left .~ (cube ^. legs . left . keep)
                CounterClockwise ->
                        cube
                        & head . left .~ (cube ^. torso . left . keep)
                        & torso . left .~ (cube ^. legs . left . keep)
                        & legs . left .~ (cube ^. feet . left . keep)
                        & feet . left .~ (cube ^. head . left . rev)
            & leftArm %~ rotate dir


rotate :: Direction -> Face -> Face
rotate Clockwise (a, b, c, d, e, f, g, h, i) = (g, d, a, h, e, b, i, f, c)
rotate CounterClockwise (a, b, c, d, e, f, g, h, i) = (c, f, i, b, e, h, g, d, a)