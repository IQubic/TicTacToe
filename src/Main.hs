module Main where
import Data.Array

data Piece = X | O | Blank

type Board = Array (Int, Int) Piece

showPiece :: Piece -> Char
showPiece X = 'X'
showPiece O = 'O'
showPiece Blank = ' '

main :: IO ()
main = putStrLn "hello world"
