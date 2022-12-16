module WordSearch.Tools
    (
        parsePuzzle
    ,	findWord
    ) where


import Data.List (transpose, findIndex, isInfixOf, isPrefixOf, tails, nub)
import Data.Maybe (fromJust)
import Data.Generics.Aliases (orElse)

type Letter = (Char, Int, Int)

findSublistIndex :: Eq a => [a] -> [a] -> Maybe Int
findSublistIndex xss xs = findIndex (isPrefixOf xss) $ tails xs

mapIndex :: (a -> Int -> b) -> [a] -> [b]
mapIndex f l = map (uncurry f) $ zip l [0..length l - 1]

findWord :: String -> [[Letter]] -> [Letter]
findWord s p = onlyString s $ head $ filter (checkIn s) (rows p ++ cols p ++ dial p ++ diar p ++ dial' p ++ diar' p)
    where lts = map (\(c, _, _) -> c)
          checkIn s line = s `isInfixOf` lts line || reverse s `isInfixOf` lts line
          indexOfR s line = fromJust $ findSublistIndex s (lts line) `orElse` findSublistIndex (reverse s) (lts line)
          onlyString s line = take (length s) $ drop (indexOfR s line) $ line
          rows p = p
          cols p = transpose p
          dial p = transpose $ mapIndex (flip drop) p
          dial' p = transpose $ mapIndex (flip drop) (reverse p)
          diar p = transpose $ mapIndex (flip drop) (map reverse p)
          diar' p = transpose $ mapIndex (flip drop) (map reverse (reverse p))

parsePuzzle :: String -> ([[Letter]], [String])
parsePuzzle p = (parseLetters $ lines p, parseWords $ lines p)
    where dup f a = f a a
          parseLine chars y = mapIndex (\[c] x -> (c, x, y)) $ words chars
          parseLetters = mapIndex parseLine . dup (take . (subtract 2) . length)
          parseWords = words . last