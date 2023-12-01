{-# LANGUAGE TemplateHaskell #-}

import Data.Char (isDigit)
import Data.FileEmbed (embedStringFile)
import Data.Maybe (catMaybes, isJust)

writtenDigits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

input :: String
input = $(embedStringFile "input/day1.txt")

mapWithIndex :: ((a, Int) -> b) -> [a] -> [b]
mapWithIndex mapper list = zipWith (curry mapper) list [0 ..]

filterFirstAndLastDigits :: String -> String
filterFirstAndLastDigits line =
  let filteredIndices = catMaybes $ mapWithIndex (\(item, index) -> if isDigit item then Just index else Nothing) line
   in [line !! (head filteredIndices), line !! (last filteredIndices)]

readToInt :: String -> Integer
readToInt = read

main :: IO ()
main = do
  print . sum . map (readToInt . filterFirstAndLastDigits) $ lines input
