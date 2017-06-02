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

mkMove :: Board -> (Int, Int) -> Piece -> Board
mkMove board spot piece = board // [(spot, piece)]

isValidMove :: Board -> (Int, Int) -> Bool
isValidMove board spot = board ! spot == Blank

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

takeInput :: (String -> Maybe a) -> String -> IO a
takeInput parser errorMsg = do input <- getLine
                               case parser input of
                                 Just num -> pure num
                                 Nothing -> putStr errorMsg >> takeInput parser errorMsg

takeMove :: Int -> IO (Int, Int)
takeMove size = do putStr "Enter a row: "
                   row <- takeInput parser (errorMsg "row")
                   putStr "Enter a col: "
                   col <- takeInput parser (errorMsg "column")
                   return (row, col)
                     where parser = parseInput size
                           errorMsg s = "Invalid " ++ s ++ ". Try again: "

takeValidMove :: Board -> IO (Int, Int)
takeValidMove board = do move <- takeMove $ getBoardSize board
                         if isValidMove board move then return move else putStrLn "Invalid Move. Try Again:" >> takeValidMove board

main :: IO ()
main = do board <- pure $ mkMove (mkNewBoard 3) (1, 1) O
          putStrLn (showBoard board)
          move <- takeValidMove board
          board' <- pure $ mkMove board move X
          putStrLn (showBoard board')
