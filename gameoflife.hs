{-# LANGUAGE NamedFieldPuns #-}
module Main where
import Data.Set (empty, union, fromList, Set, elemAt, deleteAt, member, singleton )
import System.Console.ANSI (clearScreen, setCursorPosition, hideCursor)
import System.IO ( hGetContents, IOMode(ReadMode), withFile )
import Data.Maybe ( fromJust, isNothing )
import Control.DeepSeq ( ($!!), force ) 
import Text.Read ()
import Control.Monad ()
import Control.Concurrent (threadDelay)


type Position = (Int, Int)
type Size = (Int, Int)
data GamePlan = GamePlan { width :: Int
                         , height :: Int
                         , field :: Set Position
                         } deriving Show

parsePlanSize :: [String] -> Size
parsePlanSize [width, height] = (read width, read height)
parsePlanSize _               = (0, 0)

parseInitialState :: [String] -> Int -> Set Position
parseInitialState []    _ = empty
parseInitialState (h:t) r = parseInitialState' h r `union` parseInitialState t (r + 1)
    
parseInitialState' :: String -> Int -> Set Position
parseInitialState' row index = fromList [(col, index) | col <- [0.. length row - 1], (!!) row col == '#']

readContent :: FilePath -> IO String
readContent filepath = withFile filepath ReadMode $ \h -> do 
    contents <- hGetContents h
    return $!! force contents

parseInputFile :: FilePath -> IO GamePlan
parseInputFile filepath = do
    contents <- readContent filepath
    let plan = lines contents
    let (width, height) = parsePlanSize $ take 2 plan
    let initState = parseInitialState (drop 2 plan) 0
    return $ GamePlan { width = width, height = height, field = initState }

-- prints gameplan to the terminal
printToTerminal:: GamePlan -> IO ()
printToTerminal (GamePlan {width, height, field}) = do
    if not (null field) then do
        let (x, y) = elemAt 0 field
        setCursorPosition y x
        putStrLn "#"
        let newField = deleteAt 0 field
        printToTerminal (GamePlan {width = width, height = height, field = newField})
                      else do return ()

-- for a given game plan and a cell position returns weather it is alive or not
isAlive:: GamePlan -> Position -> Bool
isAlive (GamePlan{width=w, height=h, field=inputSet}) (x, y)
    | x >= w = False
    | x < 0 = False
    | y < 0 = False
    | y >= h = False
    | otherwise = member (x, y) inputSet

-- counts the number of occurances of alive adjascent cells
numTimesFound :: Ord a => a -> [a] -> Int
numTimesFound _ [] = 0
numTimesFound x xs = (length . filter (== x)) xs

-- for a given posiiton returns number of alive adjascent cells
countAdjascentAlive:: GamePlan -> Position -> Int
countAdjascentAlive gamePlan (x, y) = do
    let positions = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1,y+1), (x,y+1), (x+1,y+1)]
    numTimesFound True (map (isAlive gamePlan) positions)

-- increases x coord until the end of line
-- the resets x coord and increases y coord
-- returns nothing if at the end of the last line
nextPosition:: Size -> Position -> Maybe Position
nextPosition (w, h) (x, y) 
    | x + 1 < w = Just (x + 1, y)
    | y + 1 < h = Just (0, y + 1)
    | otherwise = Nothing

-- takes game plan, applies the rules of the game of life
-- and returns the new plan
nextGamePlan:: GamePlan -> GamePlan
nextGamePlan gamePlan = do
    let newField = nextState gamePlan (0, 0) empty
    gamePlan{field = newField}

-- iterates through all the positions (cells) in the game plan
-- for each position it counts adjascent alive cells
-- based on the state of the current cell it determines wheather is should die, stay
-- as it was or become alive
-- Alive cells for the next state of the game plan are saved in the buildingSet / nextSet
nextState:: GamePlan -> Position -> Set Position -> Set Position
nextState gamePlan@(GamePlan{width=w, height=h, field=inputSet}) (x, y) buildingSet = do
    let adjAlive = countAdjascentAlive gamePlan (x, y)
    let nextPos = nextPosition (w, h) (x, y)
    if member (x, y) inputSet 
        then do
            let nextSet = if adjAlive == 2 || adjAlive == 3
                then buildingSet `union` singleton (x, y)
                else buildingSet
            if isNothing nextPos
                then nextSet
                else nextState gamePlan (fromJust nextPos) nextSet
        else do
            let nextSet = if adjAlive == 3
                then buildingSet `union` singleton (x, y)
                else buildingSet
            if isNothing nextPos
                then nextSet
                else nextState gamePlan (fromJust nextPos) nextSet

-- each loop of the simulation consists of clearing the screen
-- printing the plan
-- creating the next state of the game and calling the next iteration of the simulation
loop:: GamePlan -> IO ()
loop gamePlan = do
    clearScreen
    printToTerminal gamePlan
    threadDelay 200000
    (loop . nextGamePlan) gamePlan

-- reads input from the user (path to the input file), creates game plan and starts the simulation
main :: IO ()
main = do
    input <- getLine
    gamePlan <- parseInputFile input
    hideCursor
    loop gamePlan
    