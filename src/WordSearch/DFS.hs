module WordSearch.DFS
    (
        dfs
    ) where

import WordSearch.Trie(Trie)


dfs :: (Int, Int) -> Trie Char -> Char -> [Char]
dfs index trie word = [word]