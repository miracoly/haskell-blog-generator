-- Main.hs

import Html

main :: IO ()
main = putStrLn $ render myHtml

myHtml :: Html
myHtml =
  html_
    "My title" $
    append_
      (h1_ "My headline") $
      append_
        (p_ "Let's learn about haskell")
        (p_ "Paragraph 2")
