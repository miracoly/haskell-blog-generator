-- HsBlog/Html/Internal.hs

module HsBlog.Html.Internal (module HsBlog.Html.Internal) where

import Numeric.Natural (Natural)

-- * Types

newtype Html = Html String

newtype Head = Head String

newtype Structure = Structure String

newtype Content = Content String

type Title = String

-- * EDSL

html_ :: Head -> Structure -> Html
html_ (Head htmlHead) content =
  Html $
    el "html" $
      el "head" htmlHead
        <> el "body" (getStructureString content)

-- * Headings

title_ :: String -> Head
title_ = Head . el "title" . escape

stylesheet_ :: FilePath -> Head
stylesheet_ file = Head $ elAttr "link" ("href=\"" <> escape file <> "\" rel=\"stylesheet\"") ""

meta_ :: String -> String -> Head
meta_ name content =
  Head $ "<meta name=\"" <> escape name <> "\" content=\"" <> escape content <> "\">"

instance Semigroup Head where
  Head s1 <> Head s2 = Head $ s1 <> s2
  
instance Monoid Head where
  mempty = Head ""

-- * Structure

p_ :: Content -> Structure
p_ = Structure . el "p" . getContentString

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

h_ :: Natural -> Content -> Structure
h_ n = Structure . el ("h" <> show n) . getContentString

ul_ :: [Structure] -> Structure
ul_ = htmlList_ "ul"

ol_ :: [Structure] -> Structure
ol_ = htmlList_ "ol"

htmlList_ :: String -> [Structure] -> Structure
htmlList_ tag = Structure . el tag . concatMap (getStructureString . li_)

li_ :: Structure -> Structure
li_ = Structure . el "li" . getStructureString

instance Semigroup Structure where
  (<>) s1 s2 = Structure (getStructureString s1 <> getStructureString s2)

instance Monoid Structure where
  mempty = Structure ""

-- * Content

txt_ :: String -> Content
txt_ = Content . escape

link_ :: FilePath -> Content -> Content
link_ path content =
  Content $
    elAttr "a" ("href=\"" <> escape path <> "\"") (getContentString content)

img_ :: FilePath -> Content
img_ path =
  Content $ "<img src=\"" <> escape path <> "\">"

b_ :: Content -> Content
b_ = Content . el "b" . getContentString

i_ :: Content -> Content
i_ = Content . el "i" . getContentString

instance Semigroup Content where
  (<>) c1 c2 =
    Content (getContentString c1 <> getContentString c2)

instance Monoid Content where
  mempty = Content ""

-- * Render

render :: Html -> String
render html =
  case html of
    Html str -> str

-- * Utilities

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

elAttr :: String -> String -> String -> String
elAttr tag attrs content =
  "<" <> tag <> " " <> attrs <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString struct =
  case struct of
    Structure str -> str

getContentString :: Content -> String
getContentString content =
  case content of
    Content str -> str

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
