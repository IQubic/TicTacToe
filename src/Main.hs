module Main where
import Control.Monad
import Text.Read
import Data.List
import Data.List.Split
import Data.Array

data Piece = X | O | Blank deriving (Eq, Show)

data Winner = XWon | OWon | Tie | StillPlaying deriving (Show)

type Board = Array (Int, Int) Piece

mkNewBoard :: Int -> Int -> Board
mkNewBoard n m = listArray ((0, 0), (n - 1, m - 1)) (replicate (n*m) Blank)

mkMove :: Board -> (Int, Int) -> Piece -> Board
mkMove board spot piece = board // [(spot, piece)]

getBoardSize :: Board -> (Int, Int)
getBoardSize board = (colLength + 1, rowLength + 1)
  where (colLength, rowLength) = snd $ bounds board

isOnBoard :: Board -> (Int, Int) -> Bool
isOnBoard board = inRange (bounds board)

checkForWinner :: (Int, Int) -> Int -> Board -> Winner
checkForWinner spot runLength board
  | any (>= runLength) lineLengths = if lastPlayer == X then XWon else OWon
  | Blank `elem` elems board = StillPlaying
  | otherwise = Tie
  where lastPlayer = board ! spot
        lineLengths = map lineCount [(-1, -1), (-1, 0), (-1, 1), (0, 1)]
        lineCount dir@(dirX, dirY) = scanDir dir + scanDir (negate dirX, negate dirY) - 1
        scanDir dir = checkDir spot dir lastPlayer 0 board

checkDir :: (Int, Int) -> (Int, Int) -> Piece -> Int -> Board -> Int
checkDir curSpot@(curSpotX, curSpotY) dir@(dirX, dirY) player pieceCount board = if isOnBoard board curSpot && board ! curSpot == player
                                                  then checkDir (curSpotX + dirX, curSpotY + dirY) dir player (pieceCount + 1) board
                                                  else pieceCount

showPiece :: Piece -> Char
showPiece X = 'X'
showPiece O = 'O'
showPiece Blank = ' '

showBoard :: Board -> String
showBoard b = unlines $ intersperse hLines rows
              where hLines = replicate (rowLength * 2 - 1) '-'
                    rows = map (intersperse '|') $ chunksOf rowLength pieces
                    pieces = map showPiece $ elems b
                    rowLength = snd $ getBoardSize b

isValidMove :: Board -> (Int, Int) -> Bool
isValidMove board spot = board ! spot == Blank

takeInput :: (String -> Maybe a) -> String -> IO a
takeInput parser errorMsg = do input <- getLine
                               case parser input of
                                 Just num -> pure num
                                 Nothing -> putStr errorMsg >> takeInput parser errorMsg

takeMove :: (Int, Int) -> IO (Int, Int)
takeMove size = do putStr "Enter a row: "
                   row <- takeInput parseRow (errorMsg "row")
                   putStr "Enter a col: "
                   col <- takeInput parseCol (errorMsg "column")
                   return (row, col)
                     where parseRow = parseInput $ fst size
                           parseCol = parseInput $ snd size
                           parseInput limit input = (readMaybe input :: Maybe Int) >>= \num -> guard (0 <= num && num < limit) >> pure num
                           errorMsg s = "Invalid " ++ s ++ ". Try again: "

takeValidMove :: Board -> IO (Int, Int)
takeValidMove board = do move <- takeMove $ getBoardSize board
                         if isValidMove board move then return move else putStrLn "Invalid Move. Try Again:" >> takeValidMove board

main :: IO ()
main = do let board = mkNewBoard 3 3
          gameLoop 3 board X

gameLoop :: Int -> Board -> Piece -> IO ()
gameLoop runLength state piece = do putStrLn $ showPiece piece:" to play:"
                                    putStr $ showBoard state
                                    move <- takeValidMove state
                                    let state' = mkMove state move piece
                                    let winner = checkForWinner move runLength state'
                                    case winner of
                                      XWon -> putStrLn (showBoard state') >> putStrLn "X Wins!"
                                      OWon -> putStrLn (showBoard state') >> putStrLn "O Wins!"
                                      Tie  -> putStrLn (showBoard state') >> putStrLn "Tie"
                                      StillPlaying -> if piece == X
                                                      then gameLoop runLength state' O
                                                      else gameLoop runLength state' X
