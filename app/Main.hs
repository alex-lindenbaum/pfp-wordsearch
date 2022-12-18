{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Lib (parsePuzzle', mkTrie, dfs)
import System.Environment(getArgs, getProgName)
import System.Exit(die);
import Data.List(nub)
import qualified Data.Map as Map

main :: IO()
main = do
    args <- getArgs
    filename <- case args of
        [filename] -> return filename
        _ -> do
            pn <- getProgName
            die $ "Usage: " ++ pn ++ " <filename>"
    puzzle <- readFile filename
    let (p, w) = parsePuzzle' puzzle
        trie = mkTrie w
        words = Map.foldrWithKey (\index val wordList -> dfs index trie val ++ wordList) [] p
    -- foldl (\pos word -> dfs word p ++ pos) [] w
    --     positions = nub $ foldl (\pos word -> findWord word p ++ pos) [] w
    -- mapM_ (\ row -> do
    --          mapM_ (\ l@(c, _, _) ->
    --                   if l `notElem` positions
    --                   then putChar c
    --                   else putStr ("\x1b[32m" ++ [c] ++ "\x1b[0m")
    --                ) row
    --          putStrLn ""
    --       ) p
    print (Map.lookup (1, 2) p)

