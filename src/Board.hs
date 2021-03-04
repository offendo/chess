-- | Handles functionality for displaying and manipulating the board
module Board where

import Data.Either
import Data.Matrix
import Data.Void
import Piece
import Text.Megaparsec

-- | The board is a matrix of pieces
type Board = Matrix Piece

-- | Initializes an empty board with no pieces
initBoard :: Board
initBoard = matrix 8 8 start
  where
    start (2, j) = Pawn White
    blank (7, j) = Pawn White

fromFen :: String -> Either (ParseErrorBundle String Void) Board
fromFen f = case parse fen "source" f of
  Left y -> Left y
  Right x -> Right $ fromList 8 8 $ concat x

-- | Alias for Board version of prettyMatrix
showBoard :: Board -> String
showBoard = prettyMatrix
