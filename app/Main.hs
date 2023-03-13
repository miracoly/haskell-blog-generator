-- Main.hs

import Html

main :: IO ()
main = putStrLn $ render myHtml

myHtml :: Html
myHtml =
  html_
    "My title"
    $ append_
      (h1_ "My headline")
      $ append_
        (p_ "Let's learn about haskell")
        $ append_
        ( ul_
            [ p_ "item 1",
              p_ "item 2",
              p_ "item 3"
            ]
        )
        $ append_
          ( ol_
              [ p_ "item 1",
                p_ "item 2",
                p_ "item 3"
              ]
          )
          (code_ "This is a code block")
