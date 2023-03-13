-- Html.hs

module Html
  ( Html,
    Structure,
    Title,
    html_,
    p_,
    h1_,
    append_,
    render,
  )
where

newtype Html = Html String

newtype Structure = Structure String

type Title = String

-- * EDSL

html_ :: Title -> Structure -> Html
html_ title content =
  Html $
    el "html" $
      el "head" (el "title" ( escape title))
        <> el "body" (getString content)

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

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
