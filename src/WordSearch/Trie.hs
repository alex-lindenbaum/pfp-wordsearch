{-
Implements a Trie; credit: http://mchaver.com/posts/2018-12-27-tries-in-haskell.html
-}

module WordSearch.Trie
    (
        mkTrie
    ,   Trie
    ) where


import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)

data Trie a = Trie Bool (Map.Map a (Trie a)) deriving (Eq, Read, Show)

empty :: Trie a
empty = Trie False Map.empty

insert :: Ord a => [a] -> Trie a -> Trie a
insert []     (Trie _ nodes)     = Trie True nodes
insert (x:xs) (Trie end nodes) = Trie end (Map.alter (Just . insert xs . fromMaybe empty) x nodes)

-- Takes a list of words, makes into a trie
mkTrie :: Ord a => [[a]] -> Trie a
mkTrie as = mkTrie' as empty
  where
    mkTrie' []     trie = trie
    mkTrie' (x:xs) trie = mkTrie' xs $ insert x trie


member :: Ord a => [a] -> Trie a -> Bool
member []     (Trie end _) = end
member (x:xs) (Trie _ nodes) = fromMaybe False (member xs <$> Map.lookup x nodes)


lengthOfChildNodes :: Trie a -> Int
lengthOfChildNodes (Trie _ nodes) = Map.size nodes

deletable :: Ord a => [a] -> Trie a -> Bool
deletable []       (Trie _ nodes) = Map.null nodes
deletable (x : xs) (Trie end nodes) =
  (length xs == 0 || not end) &&
  maybe False (\t -> deletable xs t && (length xs == 0 || (lengthOfChildNodes t) < 1)) (Map.lookup x nodes)

delete :: Ord a => [a] -> Trie a -> Trie a
delete as t = if member as t then delete' as t else t
  where
    delete' as@(x : xs) t@(Trie end nodes) =
      if deletable as t
        then Trie end (Map.delete x nodes)
        else Trie end (Map.alter (Just . delete' xs . fromMaybe empty) x nodes)
    delete' [] t@(Trie end nodes) = if Map.size nodes > 0 then Trie False nodes else t