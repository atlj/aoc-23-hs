{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed (embedStringFile)

input :: String
input = $(embedStringFile "input/day0.txt")

main :: IO ()
main = do
  print "Hi mom"
