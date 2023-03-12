module Lib
  ( module Lib,
  )
where
  
newtype Html = Html String
newtype Structure = Structure String

html_ :: String -> Structure -> Html
html_ title content =
  Html $
    el "html" $
      el "head" (el "title" title) <>
      el "body" (getString content)

p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . el "h1"

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

append_ :: Structure -> Structure -> Structure
append_ (Structure str1) (Structure str2) = Structure (str1 <> str2)

getString :: Structure -> String
getString struct =
  case struct of
    Structure str -> str

render :: Html -> String
render html =
  case html of
    Html str -> str
