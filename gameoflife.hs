{-# LANGUAGE NamedFieldPuns #-}

module Main where
import Data.Set ( empty, union, fromList, Set, elemAt, deleteAt, member )
import System.Console.ANSI ( setCursorPosition, hideCursor, clearScreen )
import Control.Concurrent ( threadDelay )

type Coords = (Int, Int)
data GamePlan = GamePlan { width  :: Int,
                           height :: Int,
                           state  :: Set Coords } 

-- | The function returns a set of coordinates corresponding to living cells
-- | The outer function takes list of rows read from the provided file that will be parsed 
--   and the index of the first row in the list that will be parsed durring this call 
-- | The inner function parses a specific row, such that it generates coordinates 
--   that correspond to positions with '#' in the row 
parseInitialState :: [String] -> Int -> Set Coords
parseInitialState []   _     = empty
parseInitialState rows index = parseInitialStateRec (head rows) index `union` parseInitialState (tail rows) (index + 1)
    where parseInitialStateRec row i = fromList [(col, i) | col <- [0.. length row - 1], (!!) row col == '#']

-- | The function parses the given input file and returns its corresponding initial instance of a game plan
parseInputFile :: FilePath -> IO GamePlan
parseInputFile filepath = do
    contents <- readFile filepath
    let lines' = lines contents
    let (width, height) = (\x -> (head x, (head . tail) x)) (map read (take 2 lines'))
    let initState = parseInitialState (drop 2 lines') 0
    return GamePlan { width = width, height = height, state = initState }

-- | The function prints the given game plan to the terminal, such that it iterates only over the living cells 
--   and prints the '#' symbol at their corresponding position    
printGamePlan:: GamePlan -> IO ()
printGamePlan gp@GamePlan { state } = do
    if null state
        then return ()
        else do
            let (x, y) = elemAt 0 state
            setCursorPosition y x
            putStrLn "#"
            printGamePlan gp { state = deleteAt 0 state }

-- | The function returns the number of living cells around the given cell in the given game plan
countLivingNeighbours:: GamePlan -> Coords -> Int
countLivingNeighbours gp (x, y) = 
    let neighbours = [(x + a, y + b) | a <- [-1..1], b <- [-1..1], a /= 0 || b /= 0]
     in (length . filter (isAlive gp)) neighbours

-- | The function takes the current game plan and returns a new one with the next state of current
nextGamePlan:: GamePlan -> GamePlan
nextGamePlan gp = gp {state = nextState gp}
    where nextState gp'@GamePlan { width, height } = fromList [(row, col) | 
            col <- [0.. width - 1], row <- [0.. height - 1], willBeAlive gp' (row, col)]

-- | The function takes a game plan and coordinates of a cell and returns True if the specified cell 
--   is alive in the given game plan, otherwise returns False
isAlive :: GamePlan -> Coords -> Bool
isAlive GamePlan { width, height, state } (row, col) = 
    not (row >= width || row < 0 || col < 0 || col >= height) && member (row, col) state

-- | The function takes a game plan and coordinates of a cell and returns True if the specified cell
--   will be alive in the next state applying the rules of the Game of Life
willBeAlive :: GamePlan -> Coords -> Bool
willBeAlive gp coords = let livingNeighbours = countLivingNeighbours gp coords
                         in if isAlive gp coords 
                                then livingNeighbours `elem` [2,3]
                                else livingNeighbours == 3 

-- | The function simulates a one step of the game
simulate:: GamePlan -> IO ()
simulate gamePlan = do
    clearScreen
    printGamePlan gamePlan
    threadDelay 200000
    simulate $ nextGamePlan gamePlan

-- | The main function reads the input file path, parses it and call the simulation function 
main :: IO ()
main = do
    filePath <- getLine
    gamePlan <- parseInputFile filePath
    hideCursor
    simulate gamePlan