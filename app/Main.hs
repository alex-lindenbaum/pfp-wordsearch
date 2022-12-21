{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Lib (parsePuzzle, splitInQuarter, mkTrie, dfs)
import Data.List(nub)
import System.Environment(getArgs, getProgName)
import System.Exit(die);
import Data.List(union, foldl')
import Control.Parallel.Strategies
import qualified Data.Map as Map
import Control.DeepSeq

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
      output = runEval $ do
        let result = parMap rdeepseq (\(index, _) -> dfs p trie index [] "") (Map.toList p)
        -- Alternative attempt using parBuffer:
        -- let result = (withStrategy (parBuffer 16 rpar) . map (\(index, _) -> dfs p trie index [] "")) (Map.toList p)
        _ <- rseq result
        return result
  print $ last output
  print $ foldl' union [] output

  -- Alternative attempt using static partitioning into 4 segments:
  -- let (p, w) = parsePuzzle puzzle
  --     trie = mkTrie w
  --     dfsWrapper (index, _) = dfs p trie index [] ""
  --     (q1, q2, q3, q4) = splitInQuarter (Map.toList p)
  --     (p1, p2, p3, p4) = runEval $ do
  --       q1Result <- rpar (force (map dfsWrapper q1))
  --       q2Result <- rpar (force (map dfsWrapper q2))
  --       q3Result <- rpar (force (map dfsWrapper q3))
  --       q4Result <- rpar (force (map dfsWrapper q4))
  --       _ <- rseq q1Result
  --       _ <- rseq q2Result
  --       _ <- rseq q3Result
  --       _ <- rseq q4Result
  --       return (q1Result, q2Result, q3Result, q4Result)
  -- -- print $ nub $ concat (p1 ++ p2 ++ p3 ++ p4)
