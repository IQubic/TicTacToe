module Main where
import Control.Monad
import Text.Read
import Data.List
import Data.List.Split
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

showBoard :: Board -> String
showBoard b = unlines $ intersperse hLines rows
              where hLines = replicate (size * 2 - 1) '-'
                    rows = map (intersperse '|') $ chunksOf size pieces
                    pieces = map showPiece $ elems b
                    size = getBoardSize b

parseInput :: Int -> String -> Maybe Int
parseInput size input = maybeNum >>= \num -> guard (0 <= num && num < size) >> pure num
  where maybeNum = readMaybe input :: Maybe Int

main :: IO ()
main = putStrLn "hello world"
