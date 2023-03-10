module Cube(Color(..), Face(..), Cube(..), initCube) where

import Relude

data Color = Yellow | Red | Green | White | Orange | Blue
    deriving stock (Eq)

data Face = Face
    { top :: (Color, Color, Color)
    , mid :: (Color, Color, Color)
    , bot :: (Color, Color, Color)
    }
    deriving stock (Eq)

data Cube = Cube
    { head :: Face
    , torso :: Face
    , right :: Face
    , left :: Face
    , legs :: Face
    , feet :: Face
    } deriving stock (Eq)

allSame :: Color -> Face
allSame c = Face {
    top = (c,c,c),
    mid = (c,c,c),
    bot = (c,c,c)
}

initCube :: Cube
initCube = Cube
    { head = allSame Red
    , torso = allSame Yellow
    , right = allSame Blue
    , left = allSame Green
    , legs = allSame Orange
    , feet = allSame White
    }