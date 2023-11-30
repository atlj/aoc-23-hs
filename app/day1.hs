{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed (embedStringFile)

input :: String
input = $(embedStringFile "input/day1.txt")

main :: IO ()
main = do
  print "Hi mom"
