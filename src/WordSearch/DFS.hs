module WordSearch.DFS
  (
    dfs
  ) where

import WordSearch.Trie(Trie, getTrie, empty)
import WordSearch.Tools(GridMap, Coord)
import Data.Map(member, lookup)
import Data.Maybe ( fromMaybe )

dfs :: GridMap -> Trie Char -> Coord -> [Coord] -> String -> [String]
dfs p trie index@(row, col) visited word = 
  case Data.Map.lookup index p of
    Nothing -> []
    Just c  | index `elem` visited || not (member c children) -> []
            | isWord                                          -> newWord : recurse
            | otherwise                                       -> recurse
            where
              (_, children) = getTrie trie
              (isWord, _)   = getTrie child
              newWord       = word++[c]
              child         = fromMaybe empty (Data.Map.lookup c children)
              trav ind      = dfs p child ind (index:visited) newWord
              recurse       = trav (row, col-1) ++ trav (row, col+1) ++ trav (row-1, col) ++ trav (row+1, col)