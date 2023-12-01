{-# LANGUAGE TemplateHaskell #-}

import Data.Char (isDigit)
import Data.FileEmbed (embedStringFile)
import Data.List (elemIndex)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Text (pack)
import Data.Text.Internal.Search (indices)
import GHC.OldList (sortBy)

writtenDigits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

digitFromWrittenDigit :: String -> Char
digitFromWrittenDigit writtenDigit = head . show . fromJust $ writtenDigit `elemIndex` writtenDigits

writtenDigitIndices :: String -> [(Char, Int)]
writtenDigitIndices text = [(digitFromWrittenDigit search, foundIndex) | search <- writtenDigits, foundIndex <- indices (pack search) $ pack text]

input :: String
input = $(embedStringFile "input/day1.txt")

mapWithIndex :: ((a, Int) -> b) -> [a] -> [b]
mapWithIndex mapper list = zipWith (curry mapper) list [0 ..]

filterFirstAndLastDigits :: String -> String
filterFirstAndLastDigits line =
  let filteredIndices = catMaybes $ mapWithIndex (\(item, index) -> if isDigit item then Just (item, index) else Nothing) line
      filteredSortedIndices = sortBy (\(_, a) (_, b) -> compare a b) (filteredIndices ++ writtenDigitIndices line)
   in [fst (head filteredSortedIndices), fst (last filteredSortedIndices)]

readToInt :: String -> Integer
readToInt = read

main :: IO ()
main = do
  print . sum . map (readToInt . filterFirstAndLastDigits) $ lines input
  print $ writtenDigitIndices "onetwo2942four"
  print $ digitFromWrittenDigit "two"
