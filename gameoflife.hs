{-# LANGUAGE NamedFieldPuns #-}
module Main where
import Data.Set (empty, union, fromList, Set, elemAt, deleteAt, member, singleton )
import System.Console.ANSI (clearScreen, setCursorPosition, hideCursor)
import System.IO
import Data.Maybe
import Text.Read (readMaybe)
import Control.Monad (guard)
import Control.Concurrent (threadDelay)

data GamePlan = GamePlan { width :: Int
                         , height :: Int
                         , field :: Set (Int, Int)
                         } deriving Show

parsePlanSize :: [String] -> Maybe (Int, Int)
parsePlanSize lines = do  
    guard (length lines == 2)
    width <- readMaybe $ head lines
    height <- readMaybe $ last lines
    return (width, height)


parseInitialState :: [String] -> Int -> Set (Int, Int)
parseInitialState [] _= empty
parseInitialState (h:t) r = parseInitialState' h r `union` parseInitialState t (r + 1)
    
parseInitialState' :: String -> Int -> Set (Int, Int)
parseInitialState' row index = fromList [(index, col) | col <- [0.. length row - 1], (!!) row col == '#']

parseInputFile :: FilePath -> IO (Maybe GamePlan)
parseInputFile filepath = do
    handle <- openFile filepath ReadMode
    contents <- hGetContents handle
    print contents
    hClose handle
    let plan = lines contents
    let Just (width, height) = parsePlanSize $ take 2 plan
    let initState = parseInitialState (drop 2 plan) 0
    return $ Just GamePlan { width = width, height = height, field = initState }

printToTerminal:: GamePlan -> IO ()
printToTerminal (GamePlan {width, height, field}) = do
    if not (null field) then do
        let (x, y) = elemAt 0 field
        setCursorPosition x y
        putStrLn "#"
        let newField = deleteAt 0 field
        printToTerminal (GamePlan {width = width, height = height, field = newField})
                      else do return ()

isAlive:: Set (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
isAlive inputSet (w, h) (x, y)
    | x > w = False
    | x < 0 = False
    | y < 0 = False
    | y > h = False
    | otherwise = member (x, y) inputSet

numTimesFound :: Ord a => a -> [a] -> Int
numTimesFound _ [] = 0
numTimesFound x xs = (length . filter (== x)) xs

countAdjascentAlive:: Set (Int, Int) -> (Int, Int) -> (Int, Int) -> Int
countAdjascentAlive inputSet (w, h) (x, y) = do
    let positions = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1,y+1), (x,y+1), (x+1,y+1)]
    numTimesFound True (map (isAlive inputSet (w, h)) positions)

nextPosition:: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
nextPosition (w, h) (x, y) 
    | x + 1 < w = Just (x + 1, y)
    | y + 1 < h = Just (0, y + 1)
    | otherwise = Nothing

nextGamePlan:: GamePlan -> GamePlan
nextGamePlan (GamePlan {width, height, field}) = do
    let newField = nextState field (width, height) (0, 0) empty
    GamePlan {width = width, height = height, field = newField}

nextState:: Set (Int, Int) -> (Int, Int) -> (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
nextState inputSet (w, h) (x, y) buildingSet = do
    let adjAlive = countAdjascentAlive inputSet (w, h) (x, y)
    let nextPos = nextPosition (w, h) (x, y)
    if member (x, y) inputSet 
        then do
            let nextSet = if adjAlive == 2 || adjAlive == 3
                then buildingSet `union` singleton (x, y)
                else buildingSet
            if isNothing nextPos
                then nextSet
                else nextState inputSet (w, h) (fromJust nextPos) nextSet
        else do
            let nextSet = if adjAlive == 3
                then buildingSet `union` (singleton (x, y))
                else buildingSet
            if isNothing nextPos
                then nextSet
                else nextState inputSet (w, h) (fromJust nextPos) nextSet

loop:: GamePlan -> IO ()
loop gamePlan = do
    clearScreen
    printToTerminal gamePlan
    threadDelay 200000
    loop (nextGamePlan gamePlan)

main = do
    x <- getLine
    gamePlan <- parseInputFile x
    --print gamePlan
    --print $ nextGamePlan (fromJust gamePlan)
    hideCursor
    loop (fromJust gamePlan)