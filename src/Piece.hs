-- | Handles all the pieces
module Piece where

import Data.Char (digitToInt, isDigit, isUpper, toLower, toUpper)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Matrix
import Data.Void
import GHC.Show (intToDigit)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Error

data Color = White | Black

instance Show Color where
  show White = "White"
  show Black = "Black"

data Piece
  = Pawn Color
  | Knight Color
  | Bishop Color
  | Rook Color
  | Queen Color
  | King Color
  | NoPiece

instance Show Piece where
  show (Pawn White) = "♙"
  show (Knight White) = "♘"
  show (Rook White) = "♖"
  show (Bishop White) = "♗"
  show (Queen White) = "♕"
  show (King White) = "♔"
  show (Pawn Black) = "♟"
  show (Knight Black) = "♞"
  show (Rook Black) = "♜"
  show (Bishop Black) = "♝"
  show (Queen Black) = "♛"
  show (King Black) = "♚"
  show NoPiece = " " -- this is a wide whitespace char

type Parser = Parsec Void String

color :: Char -> Color
color c =
  if isUpper c
    then White
    else Black

rook :: Parser Piece
rook = do
  c <- oneOf "Rr"
  return $ Rook $ color c

pawn :: Parser Piece
pawn = do
  c <- oneOf "Pp"
  return $ Pawn $ color c

knight :: Parser Piece
knight = do
  c <- oneOf "Nn"
  return $ Knight $ color c

king :: Parser Piece
king = do
  c <- oneOf "Kk"
  return $ King $ color c

queen :: Parser Piece
queen = do
  c <- oneOf "Qq"
  return $ Queen $ color c

bishop :: Parser Piece
bishop = do
  c <- oneOf "Bb"
  return $ Bishop $ color c

nopiece :: Parser [Piece]
nopiece = do
  c <- decimal
  if c > 8
    then fail "Can't have more than 8 empty spaces"
    else return $ replicate c NoPiece

piece :: Parser Piece
piece = choice [rook, pawn, knight, king, queen, bishop]

-- | Full parser for a fen string to a list of pieces 64 long
fen :: Parser [[Piece]]
fen = (:) <$> row <*> count 7 (char '/' *> row)
  where
    row = do
      pieces <- concat <$> many (choice [(: []) <$> piece, nopiece]) <?> "one of pbnrqk/PBNRQK"
      if length pieces > 8
        then fail "Error: Too many pieces!"
        else return $ take 8 $ pieces ++ repeat NoPiece

-- | Converts a list of pieces into FEN
toFen :: [[Piece]] -> String
toFen [] = ""
toFen double = intercalate "/" $ map _toFen double
  where
    _toFen [] = ""
    _toFen (p : rest) = case p of
      Rook Black -> 'r' : _toFen rest
      Rook White -> 'R' : _toFen rest
      Queen Black -> 'q' : _toFen rest
      Queen White -> 'Q' : _toFen rest
      Pawn Black -> 'p' : _toFen rest
      Pawn White -> 'P' : _toFen rest
      King Black -> 'k' : _toFen rest
      King White -> 'K' : _toFen rest
      Bishop Black -> 'b' : _toFen rest
      Bishop White -> 'B' : _toFen rest
      Knight Black -> 'n' : _toFen rest
      Knight White -> 'N' : _toFen rest
      NoPiece -> intToDigit numNoPieces : _toFen (drop (numNoPieces - 1) rest)
      where
        countNoPieces :: [Piece] -> Int
        countNoPieces [] = 0
        countNoPieces (x : rest) =
          case x of
            NoPiece -> 1 + countNoPieces rest
            _ -> 0
        numNoPieces = countNoPieces (p : rest)
