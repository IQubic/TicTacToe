module Main where
import Data.Array

data Piece = X | O | Blank deriving (Eq, Show)

type Board = Array (Int, Int) Piece

mkNewBoard :: Int -> Board
mkNewBoard n = listArray ((0, 0), (n - 1, n -1)) (replicate (n*n) Blank)

mkMove :: Board -> Int -> Int -> Piece -> Board
mkMove state n m piece = state // [((n, m), piece)]

getBoardSize :: Board -> Int
getBoardSize b = snd (snd $ bounds b) + 1

showPiece :: Piece -> Char
showPiece X = 'X'
showPiece O = 'O'
showPiece Blank = ' '

main :: IO ()
main = putStrLn "hello world"
