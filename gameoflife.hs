--{-# LANGUAGE NamedFieldPuns #-}
module Main where
import Data.Set (empty, union, fromList, Set)
import System.Console.ANSI ()
import System.IO
import Text.Read (readMaybe)
import Control.Monad (guard)

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


main = do
    x <- getLine
    gamePlan <- parseInputFile x
    print gamePlan