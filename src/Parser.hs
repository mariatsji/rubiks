module Parser (movesP, movesFromText) where

import Cube

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as AT
import Relude

movesFromText :: Text -> Either String [Move]
movesFromText = AT.parseOnly movesP

-- all the moves
movesP :: Parser [Move]
movesP = do
    ms <-
        AT.many1
            ( do
                m <- moveChunk
                _ <- AT.skipSpace
                pure m
            )
    pure (concat ms)

-- one or two moves
moveChunk :: Parser [Move]
moveChunk = do
    place <- placeParser
    twice <- twiceParser
    dir <- dirParser
    pure $
        replicate (if twice then 2 else 1) (Move place dir)

dirParser :: Parser Direction
dirParser = do
    x <- AT.peekChar
    if x == Just '\''
        then
            ( do
                _ <- AT.char '\''
                pure CounterClockwise
            )
        else pure Clockwise

twiceParser :: Parser Bool
twiceParser = do
    x <- AT.peekChar
    if x == Just '2'
        then
            ( do
                _ <- AT.char '2'
                pure True
            )
        else pure False

placeParser :: Parser Place
placeParser =
    Torso <$ AT.char 'F'
        <|> RightArm <$ AT.char 'R'
        <|> LeftArm <$ AT.char 'L'
        <|> Head <$ AT.char 'U'
        <|> Feet <$ AT.char 'B'
        <|> Legs <$ AT.char 'D'
