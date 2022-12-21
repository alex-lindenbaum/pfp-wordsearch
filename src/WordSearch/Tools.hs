module WordSearch.Tools
  (
    parsePuzzle
  , GridMap
  , Coord
  , splitInQuarter
  ) where


import Data.Map (Map, insert, empty)

type Coord = (Int, Int)
type GridMap = Map Coord Char


splitInQuarter :: [a] -> ([a], [a], [a], [a])
splitInQuarter l = (a,b,c,d)
  where (left, right) = splitInHalf l
        (a,b) = splitInHalf left
        (c,d) = splitInHalf right


splitInHalf :: [a] -> ([a], [a])
splitInHalf l = splitInHalfHelper l [] [] 0


splitInHalfHelper :: [a] -> [a] -> [a] -> Int -> ([a], [a])
splitInHalfHelper [] l r _ = (l, r)
splitInHalfHelper (x:xs) l r parity
  | parity == 0   = splitInHalfHelper xs (x:l) r 1
  | otherwise     = splitInHalfHelper xs l (x:r) 0


parsePuzzle :: String -> (GridMap, [String])
parsePuzzle p = (grid, wordList)
    where linesP = lines p
          grid = parseGrid gridStrings 0 empty
          gridStrings = take (length linesP - 2) linesP
          wordList = words . last $ linesP


parseGrid :: [String] -> Int -> GridMap -> GridMap
parseGrid [] _ grid = grid
parseGrid (x:xs) row grid = parseGrid xs (row + 1) (parseRow x (row, 0) grid)


parseRow :: String -> Coord -> GridMap -> GridMap
parseRow [] _ grid = grid
parseRow (x:xs) (row, col) grid = parseRow xs (row, col + 1) (insert (row, col) x grid)















