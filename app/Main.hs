{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Lib (parsePuzzle, splitInQuarter, mkTrie, dfs)
import Data.List(nub)
import System.Environment(getArgs, getProgName)
import System.Exit(die);
import Control.Parallel.Strategies
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
  let (p, w) = parsePuzzle puzzle
      trie = mkTrie w
      -- f index _ wordList = wordList ++ dfs p trie index [] ""
      -- output = Map.foldrWithKey f [] p
      output = runEval $ do
        let result = parMap rpar (\(index, _) -> dfs p trie index [] "") (Map.toList p)
        -- let result = (withStrategy (parBuffer 100 r) . map (\(index, _) -> dfs p trie index [] "")) (Map.toList p)

        -- parMap strat f = withStrategy (parList strat) . map f
        _ <- rseq result
        return result



  let (p, w) = parsePuzzle puzzle
      trie = mkTrie w
      dfsWrapper (index, _) = dfs p trie index [] ""
      (q1,q2,q3,q4) = splitInQuarter . Map.toList p
      output = runEval $ do
        q1Result <- rpar (dfsWrapper q1)
        q2Result <- rpar (dfsWrapper q2)
        q3Result <- rpar (dfsWrapper q3)
        q4Result <- rpar (dfsWrapper q4)
        _ <- rseq q1Result
        _ <- rseq q2Result
        _ <- rseq q3Result
        _ <- rseq q4Result
        return nub $ q1Result ++ q2Result ++ q3Result ++ q4Result

  print output
