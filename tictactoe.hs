{-# LANGUAGE FlexibleInstances,OverlappingInstances #-}
import Control.Monad
import System.Exit
--import Data.List.Split
import Data.Char (isDigit)

data Player = X | O deriving (Eq)
data GameResult = GameResult Player | Tie | Unknown
type Board = [[Maybe Player]]
type Position = (Int, Int) -- y,x
opponent X = O
opponent O = X

instance Show (Maybe Player) where
  show Nothing = "#"
  show (Just O) = "O"
  show (Just X) = "X"
  showList [] = showString ""
  showList (x:s) = showString (show x ++ " " ++ show s)

instance Show Board where
  show [] = ""
  show (row:x) = show row ++ "\n" ++ show x

instance Read (Maybe Player) where
  readsPrec _ "X" = [(Just X, "")]
  readsPrec _ "O" = [(Just O, "")]
  readsPrec _ _ = [(Nothing, "")]

instance Read (Maybe Position) where
  readsPrec _ input = 
    let 
      (yi,xs) = span isDigit input  -- split on any character that's not a digit
      (c:xi) = xs  -- take off the first character of the x portion (it's the comma)
      y = read yi :: Int
      x = read xi :: Int
    in
      if all isDigit yi && length xs > 1 && c == ','  -- lazy eval & short circuiting ensure we don't hit a runtime error
        then [(Just (y,x), "")]
        else [(Nothing, "")]

-- pseudomutability rocks
replaceElement :: [a] -> Int -> a -> [a]
replaceElement xs i x = fore ++ (x:aft)
  where (fore,aft) = (take i xs, drop (i+1) xs)

makeMove :: Player -> Position -> Board -> Board
makeMove player (y,x) board = replaceElement board y row
  where row = replaceElement (board !! y) x (Just player)

checkGame :: Board -> GameResult
checkGame board = False
-- not implemented yet

choosePlayer :: IO Player
choosePlayer = do
  putStrLn $ "TicTacToe! Choose (X/O):"
  playerStr <- getLine
  let player = read playerStr :: Maybe Player
  if player == Nothing
    then do
      putStrLn $ "Invalid choice dumbass!"
      choosePlayer
    else let (Just p) = player in return p

divide :: Int -> Int -> Int
divide a b = if a < b then 0 else (divide  (a-b) b)

chooseMove :: Player -> IO Position
chooseMove player = do
  putStrLn $ "Your Move " ++ show (Just player) ++ " (y0-2,x0-2):"
  moveStr <- getLine
  let move = read moveStr :: Maybe Position
  if move == Nothing
    then do
      putStrLn $ "Invalid move dumbass!"
      chooseMove player
    else let (Just m) = move in return m

gameLoop :: Board -> Player -> IO (Maybe Player)
gameLoop board player = do
  if checkGame board
    then return (winner board)
    else do
      move <- chooseMove player
      let newBoard = makeMove player move board
      putStrLn $ show newBoard
      gameLoop newBoard (opponent player)

main :: IO ()
main = do
    player <- choosePlayer
    let board = [[Nothing, Nothing, Nothing],
                 [Nothing, Nothing, Nothing],
                 [Nothing, Nothing, Nothing]] :: Board
    putStrLn $ show board
    champion <- gameLoop board player
    putStrLn $ "Game over, " ++ show champion ++ " won."
