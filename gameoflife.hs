{-# LANGUAGE NamedFieldPuns #-}
module Main where
import Data.Set (empty, union, fromList, Set, elemAt, deleteAt, member, singleton )
import System.Console.ANSI (clearScreen, setCursorPosition, hideCursor)
import System.IO
    ( hClose, hGetContents, openFile, IOMode(ReadMode) )
import Data.Maybe ( fromJust, isNothing )
import Text.Read (readMaybe)
import Control.Monad (guard)
import Control.Concurrent (threadDelay)


type Position = (Int, Int)
type Size = (Int, Int)
data GamePlan = GamePlan { width :: Int
                         , height :: Int
                         , field :: Set Position
                         } deriving Show

parsePlanSize :: [String] -> Size
parsePlanSize [width, height] = (read width, read height)

parseInitialState :: [String] -> Int -> Set Position
parseInitialState [] _= empty
parseInitialState (h:t) r = parseInitialState' h r `union` parseInitialState t (r + 1)
    
parseInitialState' :: String -> Int -> Set Position
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
    if not (null field) then do
        let (x, y) = elemAt 0 field
        setCursorPosition y x
        putStrLn "#"
        let newField = deleteAt 0 field
        printToTerminal (GamePlan {width = width, height = height, field = newField})
                      else do return ()

isAlive:: GamePlan -> Position -> Bool
isAlive (GamePlan{width=w, height=h, field=inputSet}) (x, y)
    | x >= w = False
    | x < 0 = False
    | y < 0 = False
    | y >= h = False
    | otherwise = member (x, y) inputSet

numTimesFound :: Ord a => a -> [a] -> Int
numTimesFound _ [] = 0
numTimesFound x xs = (length . filter (== x)) xs

countAdjascentAlive:: GamePlan -> Position -> Int
countAdjascentAlive gamePlan (x, y) = do
    let positions = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1,y+1), (x,y+1), (x+1,y+1)]
    numTimesFound True (map (isAlive gamePlan) positions)

nextPosition:: Size -> Position -> Maybe Position
nextPosition (w, h) (x, y) 
    | x + 1 < w = Just (x + 1, y)
    | y + 1 < h = Just (0, y + 1)
    | otherwise = Nothing

nextGamePlan:: GamePlan -> GamePlan
nextGamePlan gamePlan = do
    let newField = nextState gamePlan (0, 0) empty
    gamePlan{field = newField}

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

loop:: GamePlan -> IO ()
loop gamePlan = do
    clearScreen
    printToTerminal gamePlan
    threadDelay 200000
    (loop . nextGamePlan) gamePlan

main :: IO ()
main = do
    x <- getLine
    gamePlan <- parseInputFile x
    hideCursor
    loop gamePlan

{-
    print $ nextPosition (2, 3) (0, 0)
    print $ nextPosition (2, 3) (1, 0)
    print $ nextPosition (2, 3) (1, 1)
    print $ nextPosition (2, 3) (1, 2)
    let gp@GamePlan{width, height, field} = fromJust gamePlan
    print width
    print height
    print gp
    print $ countAdjascentAlive gp (0, 0) == 0
    print $ countAdjascentAlive gp (5, 0) == 2
    print $ countAdjascentAlive gp (6, 1) == 3
    print $ nextState gp (0, 0) empty
-}