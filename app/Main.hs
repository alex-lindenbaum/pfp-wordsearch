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
        -- let result = (withStrategy (parBuffer 100 rpar) . map (\(index, _) -> dfs p trie index [] "")) (Map.toList p)
        -- parMap strat f = withStrategy (parList strat) . map f
        _ <- rseq result
        return result
  print $ last output
  -- putStrLn " "
  -- print $ foldl' union [] output

  -- let (p, w) = parsePuzzle puzzle
  --     trie = mkTrie w
  --     dfsWrapper (index, _) = dfs p trie index [] ""
  --     -- solutions = parMap rseq dfsWrapper (Map.toList p)

  --     (q1,q2,q3,q4) = splitInQuarter (Map.toList p)
  --     -- pList = Map.toList p
  --     -- (t1, t2) = splitAt (length pList `div` 2) pList
  --     -- (q1, q2) = splitAt (length t1 `div` 2) t1
  --     -- (q3, q4) = splitAt (length t1 `div` 2) t2
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

  -- print $ nub $ foldl' (++) [] (p1 ++ p2 ++ p3 ++ p4)
