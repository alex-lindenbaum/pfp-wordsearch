{-
Implements a Trie; credit: http://mchaver.com/posts/2018-12-27-tries-in-haskell.html
-}

module WordSearch.Trie
  (
    mkTrie
  , Trie
  , getTrie
  , empty
  ) where


import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)

data Trie a = Trie Bool (Map.Map a (Trie a)) deriving (Eq, Read, Show)

empty :: Trie a
empty = Trie False Map.empty

getTrie :: Trie a -> (Bool, Map.Map a (Trie a))
getTrie (Trie end nodes) = (end, nodes)

insert :: Ord a => [a] -> Trie a -> Trie a
insert []     (Trie _ nodes)     = Trie True nodes
insert (x:xs) (Trie end nodes) = Trie end (Map.alter (Just . insert xs . fromMaybe empty) x nodes)

-- Takes a list of words, makes into a trie
mkTrie :: Ord a => [[a]] -> Trie a
mkTrie as = mkTrie' as empty
  where
    mkTrie' []     trie = trie
    mkTrie' (x:xs) trie = mkTrie' xs $ insert x trie