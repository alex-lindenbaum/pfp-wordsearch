{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Lib (parsePuzzle, findWord)
import System.Environment(getArgs, getProgName)
import System.Exit(die); 
import Data.List(nub)

main :: IO()
main = do
    args <- getArgs
    filename <- case args of
        [filename] -> return filename
        _ -> do
            pn <- getProgName
            die $ "Usage: " ++ pn ++ " <filename>"
    puzzle <- readFile filename
    let (p, w) = parsePuzzle puzzle
        positions = nub $ foldl (\pos word -> findWord word p ++ pos) [] w
    mapM_ (\ row -> do
             mapM_ (\ l@(c, _, _) ->
                      if l `notElem` positions
                      then putChar c
                      else putStr ("\x1b[32m" ++ [c] ++ "\x1b[0m")
                   ) row
             putStrLn ""
          ) p


