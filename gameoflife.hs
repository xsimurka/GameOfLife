{-# LANGUAGE NamedFieldPuns #-}

module Main where
import Data.Set (empty, union, fromList, Set, elemAt, deleteAt, member, singleton )
import System.Console.ANSI (clearScreen, setCursorPosition, hideCursor)
import System.IO
import Data.Maybe
import Text.Read (readMaybe)
import Control.Monad (guard)
import Control.Concurrent (threadDelay)

type Coords = (Int, Int)
data GamePlan = GamePlan {  width  :: Int,
                            height :: Int,
                            field  :: Set (Int, Int)
                            } 

parsePlanSize :: [String] -> Coords
parsePlanSize [line1, line2] = (read line1, read line2) 

parseInitialState :: [String] -> Int -> Set Coords
parseInitialState [] _ = empty
parseInitialState (h:t) r = parseInitialState' h r `union` parseInitialState t (r + 1)
    
parseInitialState' :: String -> Int -> Set Coords
parseInitialState' row index = fromList [(col, index) | col <- [0.. length row - 1], (!!) row col == '#']

parseInputFile :: FilePath -> IO GamePlan
parseInputFile filepath = do
    handle <- openFile filepath ReadMode
    contents <- hGetContents handle
    print contents
    hClose handle
    let plan = lines contents
    let (width, height) = parsePlanSize $ take 2 plan
    let initState = parseInitialState (drop 2 plan) 0
    return $ GamePlan { width = width, height = height, field = initState }

printToTerminal:: GamePlan -> IO ()
printToTerminal (GamePlan {width, height, field}) = do
    if null field
        then return ()
        else do
            let (x, y) = elemAt 0 field
            setCursorPosition y x
            putStrLn "#"
            printToTerminal $ GamePlan {width = width, height = height, field = deleteAt 0 field}

countLivingNeighbours:: GamePlan -> Coords -> Int
countLivingNeighbours gp@(GamePlan {width, height, field}) (x, y) = 
    let neighbours = [(x+a, y+b) | a <- [-1..1], b <- [-1..1], a /= 0 || b /= 0]
    in (length . filter id . map (isAlive gp)) neighbours

nextGamePlan:: GamePlan -> GamePlan
nextGamePlan gp@(GamePlan {width, height, field}) = 
    GamePlan {width = width, height = height, field = nextState gp}

nextState :: GamePlan -> Set Coords
nextState gp@(GamePlan {width, height, field}) = fromList [(row, col) | 
    col <- [0.. width-1], row <- [0.. height-1], willBeAlive gp (row, col)]

isAlive :: GamePlan -> Coords -> Bool
isAlive gp@(GamePlan {width, height, field}) (row, col) = 
    not (row >= width || row < 0 || col < 0 || col >= height) && member (row, col) field

willBeAlive :: GamePlan -> Coords -> Bool
willBeAlive gp@(GamePlan {width, height, field}) coords =
    let livingNeighbours = countLivingNeighbours gp coords
    in if isAlive gp coords 
        then livingNeighbours `elem` [2,3]
        else livingNeighbours == 3 

simulate:: GamePlan -> IO ()
simulate gamePlan = do
    clearScreen
    printToTerminal gamePlan
    threadDelay 200000
    simulate $ nextGamePlan gamePlan

main = do
    f <- getLine
    gamePlan <- parseInputFile f
    hideCursor
    simulate gamePlan