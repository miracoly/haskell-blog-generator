-- Html/Internal.hs

module Html.Internal (module Html.Internal) where
  
-- * Types

newtype Html = Html String

newtype Structure = Structure String

type Title = String

-- * EDSL

html_ :: Title -> Structure -> Html
html_ title content =
  Html $
    el "html" $
      el "head" (el "title" (escape title))
        <> el "body" (getString content)

p_ :: String -> Structure
p_ = Structure . el "p" . escape

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

ul_ :: [Structure] -> Structure
ul_ = htmlList_ "ul"

ol_ :: [Structure] -> Structure
ol_ = htmlList_ "ol"

htmlList_ :: String -> [Structure] -> Structure
htmlList_ tag = Structure . el tag . concatMap (getString . li_)

li_ :: Structure -> Structure
li_ = Structure . el "li" . getString

append_ :: Structure -> Structure -> Structure
append_ (Structure str1) (Structure str2) = Structure (str1 <> str2)

-- * Render

render :: Html -> String
render html =
  case html of
    Html str -> str
    
-- * Utilities

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
   in concatMap escapeChar

getString :: Structure -> String
getString struct =
  case struct of
    Structure str -> str
