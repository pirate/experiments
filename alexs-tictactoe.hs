module Main where

import Data.List
import Data.Maybe

newtype Player = Player Bool
    deriving(Eq)

x = Player True
o = Player False

instance Show Player where
    show (Player True) = "X"
    show (Player False) = "O"

opponent :: Player -> Player
opponent (Player p) = Player $ not p

newtype Board = Board [[Maybe Player]]

showMaybePlayer :: Maybe Player -> String
showMaybePlayer (Just p) = show p
showMaybePlayer Nothing = " "

instance Show Board where
    show (Board b) = showMaybePlayer ((b !! 0) !! 0) ++ "|" ++ showMaybePlayer ((b !! 0) !! 1) ++ "|" ++ showMaybePlayer ((b !! 0) !! 2) ++ "\n"
        ++ replicate 5 '-' ++ "\n"
        ++ showMaybePlayer ((b !! 1) !! 0) ++ "|" ++ showMaybePlayer ((b !! 1) !! 1) ++ "|" ++ showMaybePlayer ((b !! 1) !! 2) ++ "\n"
        ++ replicate 5 '-' ++ "\n"
        ++ showMaybePlayer ((b !! 2) !! 0) ++ "|" ++ showMaybePlayer ((b !! 2) !! 1) ++ "|" ++ showMaybePlayer ((b !! 2) !! 2)

emptyBoard = Board $ replicate 3 $ replicate 3 Nothing

access :: Board -> (Int, Int) -> Maybe Player
access (Board b) (x, y) = (b !! x) !! y

legal :: Board -> (Int, Int) -> Bool
legal b = isNothing . access b

move :: Player -> Board -> (Int, Int) -> Board
move p (Board b) (x, y) = Board $ take x b ++ (take y row ++ Just p : drop (y + 1) row) : drop (x + 1) b where
    row = b !! x

allMoves = [(x,y) | x <- [0..2], y <- [0..2]]

threes = [[(0,0),(0,1),(0,2)], [(1,0),(1,1),(1,2)], [(2,0),(2,1),(2,2)],
    [(0,0),(1,0),(2,0)], [(0,1),(1,1),(2,1)], [(0,2),(1,2),(2,2)],
    [(0,0),(1,1),(2,2)], [(0,2),(1,1),(2,0)]]

win :: Player -> Board -> Bool
win p b = or $ map (and . map ((== Just p) . access b)) threes

full :: Board -> Bool
full b = isNothing $ find (legal b) allMoves

winOrTie :: Player -> Board -> Bool
winOrTie p b = win p b || (full b && not (win (opponent p) b))

allLegalMoves :: Player -> Board -> [Board]
allLegalMoves p b = if win x b || win o b
    then []
    else map (move p b) $ filter (legal b) allMoves

-- assumes opposing player moves next
force :: (Board -> Bool) -> Player -> Board -> Bool
force winFunc p b = if winFunc b || (length alm > 0 && all winFunc alm)
    then True
    else length almRound2 > 0 && all (any (force winFunc p)) almRound2
    where
        alm = allLegalMoves (opponent p) b
        almRound2 = map (allLegalMoves p) alm

aiMove :: Player -> Board -> Board
aiMove p b = fromMaybe (fromJust forceTie) forceWin where
    alm = allLegalMoves p b
    forceWin = find (force (win p) p) alm
    forceTie = find (force (winOrTie p) p) alm

playRound :: Player -> Board -> IO Board
playRound humanPlayer b = do
    let newB = if humanPlayer == x
        then b
        else aiMove (opponent humanPlayer) b
    putStrLn $ show newB
    putStrLn "Enter move: (down, over):"
    moveStr <- getLine
    let newB2 = move humanPlayer newB (read moveStr)
    return $ if humanPlayer == x && not (full newB2)
        then aiMove (opponent humanPlayer) newB2
        else newB2

play :: Player -> Board -> IO ()
play humanPlayer b = do
    if win (opponent humanPlayer) b
        then putStrLn "You lose." >> putStrLn (show b)
        else if winOrTie (opponent humanPlayer) b
            then putStrLn "Cat's game." >> putStrLn (show b)
            else playRound humanPlayer b >>= play humanPlayer

main :: IO ()
main = do
    putStrLn "Choose your piece (X/O):"
    choice <- getLine
    let humanPlayer = if choice == "X"
        then x
        else o
    play humanPlayer emptyBoard
