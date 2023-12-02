{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed (embedStringFile)

input :: String
input = $(embedStringFile "input/day2.txt")

main :: IO ()
main = do
  print "Hi mom"
