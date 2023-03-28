{-# LANGUAGE NamedFieldPuns #-}

module Main where
import Data.Set (empty, union, fromList, Set, elemAt, deleteAt, member)
import System.Console.ANSI (clearScreen, setCursorPosition, hideCursor)
import Control.Concurrent (threadDelay)

type Coords = (Int, Int)
data GamePlan = GamePlan {  width  :: Int,
                            height :: Int,
                            field  :: Set (Int, Int)
                            } 

parseInitialState :: [String] -> Int -> Set Coords
parseInitialState [] _ = empty
parseInitialState (h:t) r = parseInitialStateRec h r `union` parseInitialState t (r + 1)
    where parseInitialStateRec row index = fromList [(col, index) | 
            col <- [0.. length row - 1], (!!) row col == '#']

parseInputFile :: FilePath -> IO GamePlan
parseInputFile filepath = do
    contents <- readFile filepath
    let plan = lines contents
    let (width, height) = (\x -> (head x, (head . tail) x)) (map read (take 2 plan))
    let initState = parseInitialState (drop 2 plan) 0
    return $ GamePlan { width = width, height = height, field = initState }
    
printGamePlan:: GamePlan -> IO ()
printGamePlan gp@GamePlan { field } = do
    if null field
        then return ()
        else do
            let (x, y) = elemAt 0 field
            setCursorPosition y x
            putStrLn "#"
            printGamePlan $ gp { field = deleteAt 0 field }

countLivingNeighbours:: GamePlan -> Coords -> Int
countLivingNeighbours gp (x, y) = 
    let neighbours = [(x+a, y+b) | a <- [-1..1], b <- [-1..1], a /= 0 || b /= 0]
    in (length . filter id . map (isAlive gp)) neighbours

nextGamePlan:: GamePlan -> GamePlan
nextGamePlan gp = gp {field = nextState gp}
    where nextState gp'@GamePlan { width, height } = fromList [(row, col) | 
            col <- [0.. width - 1], row <- [0.. height - 1], willBeAlive gp' (row, col)]

isAlive :: GamePlan -> Coords -> Bool
isAlive GamePlan { width, height, field } (row, col) = 
    not (row >= width || row < 0 || col < 0 || col >= height) && member (row, col) field

willBeAlive :: GamePlan -> Coords -> Bool
willBeAlive gp coords = let livingNeighbours = countLivingNeighbours gp coords
                        in if isAlive gp coords 
                            then livingNeighbours `elem` [2,3]
                            else livingNeighbours == 3 

simulate:: GamePlan -> IO ()
simulate gamePlan = do
    clearScreen
    printGamePlan gamePlan
    threadDelay 200000
    simulate $ nextGamePlan gamePlan

main :: IO ()
main = do
    filePath <- getLine
    gamePlan <- parseInputFile filePath
    hideCursor
    simulate gamePlan