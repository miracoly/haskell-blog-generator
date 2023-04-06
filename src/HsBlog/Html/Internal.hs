-- HsBlog/Html/Internal.hs

module HsBlog.Html.Internal (module HsBlog.Html.Internal) where

import Numeric.Natural (Natural)

-- * Types

newtype Html = Html String

newtype Structure = Structure String

instance Semigroup Structure where
  (<>) struc1 struc2 = Structure (getString struc1 <> getString struc2)
  
instance Monoid Structure where
  mempty = empty_

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

h_ :: Natural -> String -> Structure
h_ n = Structure . el ("h" <> show n) . escape

ul_ :: [Structure] -> Structure
ul_ = htmlList_ "ul"

ol_ :: [Structure] -> Structure
ol_ = htmlList_ "ol"

htmlList_ :: String -> [Structure] -> Structure
htmlList_ tag = Structure . el tag . concatMap (getString . li_)

li_ :: Structure -> Structure
li_ = Structure . el "li" . getString

empty_ :: Structure
empty_ = Structure ""

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
