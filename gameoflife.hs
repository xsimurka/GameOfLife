{-# LANGUAGE NamedFieldPuns #-}

import Data.Set
import System.Console.ANSI
import System.IO
import Text.Read (readMaybe)


data GamePlan = GamePlan { width :: Int
                         , height :: Int
                         , field :: Set (Int, Int)
                         } deriving Show

parsePlanSize :: [String] -> Maybe (Int, Int)
parsePlanSize lines = do  
  let planSize = do
        guard (lines == 2)
        width <- readMaybe (head lines)
        height <- readMaybe (last lines)
        return (width, height)
  return planSize

parseInitialState :: [String] -> Set (Int, Int) -> Maybe (Set (Int, Int))
parseInitialState plan set = do
    
parseInitialState' :: [String] -> Set (Int, Int) -> Maybe (Set (Int, Int))
parseInitialState' plan set = do

parseInputFile :: FilePath -> Maybe GamePlan
parseInputFile filepath = do
    handle <- openFile filepath ReadMode
    contents <- hGetContents handle
    hClose handle
    let lines = lines contents
    (width, height) <- parsePlanSize $ take 2 lines
    let plan = drop 2 lines
    guard (length plan == height && all ((== width) . len) plan)
    initState <- parseInitialState (plan) empty
    return $ GamePlan { width = width, height = height, field = initState }