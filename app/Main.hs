{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Lib (parsePuzzle', mkTrie, dfs)
import Control.Parallel.Strategies(rpar, rseq, runEval, parMap)
import System.Environment(getArgs, getProgName)
import System.Exit(die);
import qualified Data.Map as Map
import Data.List(union, foldl')

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
      output = map (\(index, _) -> dfs p trie index [] "") (Map.toList p)
      result = foldl' union [] output
      -- output = runEval $ do
      --   let result = parMap rpar (\(index, _) -> dfs p trie index [] "") (Map.toList p)
      --   _ <- rseq result
      --   return result
  print result