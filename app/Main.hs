module Main (main) where

import Lib

main :: IO ()
main = putStrLn myHtml

myHtml :: String
myHtml =
  makeHtml
    "My title" 
    (h1_ "My headline" <> p_ "Let's learn about haskell")