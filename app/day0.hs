{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed (embedStringFile)
import Test.HUnit

input :: String
input = $(embedStringFile "input/day0.txt")

testHello = TestCase $ do
  assertEqual "can say hello to John Doe" "Hello John Doe" (hello "John Doe")

hello :: String -> String
hello name = "Hello " ++ name

main' = do
  print $ hello "mom"

main :: IO ()
main = do
  result <- runTestTT $ TestList [testHello]
  main'
