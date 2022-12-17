module WordSearch.Tools
    (
        parsePuzzle
    ,   findWord
    ) where


import Data.List (transpose, findIndex, isInfixOf, isPrefixOf, tails, nub)
import Data.Map (Map, insert, empty)
import Data.Maybe (fromJust)
import Data.Generics.Aliases (orElse)

type Letter = (Char, Int, Int)
type GridMap = Map (Int, Int) Char

findSublistIndex :: Eq a => [a] -> [a] -> Maybe Int
findSublistIndex xss xs = findIndex (isPrefixOf xss) $ tails xs

mapIndex :: (a -> Int -> b) -> [a] -> [b]
mapIndex f l = zipWith f l [0..length l - 1]

findWord :: String -> [[Letter]] -> [Letter]
findWord s p = onlyString s $ head $ filter (checkIn s) (rows p ++ cols p ++ dial p ++ diar p ++ dial' p ++ diar' p)
    where lts = map (\(c, _, _) -> c)
          checkIn s line = s `isInfixOf` lts line || reverse s `isInfixOf` lts line
          indexOfR s line = fromJust $ findSublistIndex s (lts line) `orElse` findSublistIndex (reverse s) (lts line)
          onlyString s line = take (length s) $ drop (indexOfR s line) $ line
          rows p = p
          dial p = transpose $ mapIndex (flip drop) p
          dial' p = transpose $ mapIndex (flip drop) (reverse p)
          diar p = transpose $ mapIndex (flip drop) (map reverse p)
          diar' p = transpose $ mapIndex (flip drop) (map reverse (reverse p))
          cols = transpose

parsePuzzle :: String -> ([[Letter]], [String])
parsePuzzle p = (parseLetters $ lines p, parseWords $ lines p)
    where dup f a = f a a
          parseLine chars y = mapIndex (\[c] x -> (c, x, y)) $ words chars
          parseLetters = mapIndex parseLine . dup (take . subtract 2 . length)
          parseWords = words . last


parsePuzzle' :: String -> (GridMap, [String])
parsePuzzle' p = (grid, wordList)
    where linesP = lines p
          grid = parseGrid gridStrings 0 empty
          gridStrings = take (length linesP - 2) linesP
          wordList = words . last $ linesP


parseGrid :: [String] -> Int -> GridMap -> GridMap
parseGrid [] _ grid = grid
parseGrid (x:xs) row grid = parseGrid xs (row + 1) (parseRow x row 0 grid)


parseRow :: String -> Int -> Int -> GridMap -> GridMap
parseRow [] _ _ grid = grid
parseRow (x:xs) row col grid = parseRow xs row (col + 1) (insert (row, col) x grid)















