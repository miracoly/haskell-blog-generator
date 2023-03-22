-- Convert/Internal.hs

module Convert.Internal (module Convert.Internal) where

import qualified Html.Internal as Html
import qualified Markup.Internal as Markup

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure

convertStructure :: Markup.Structure -> Html.Structure
convertStructure struct =
  case struct of
    Markup.Heading n txt -> Html.h_ n txt
    Markup.Paragraph txt -> Html.p_ txt
    Markup.UnorderedList ls -> Html.ul_$ map Html.p_ ls
    Markup.OrderedList ls -> Html.ol_$ map Html.p_ ls
    Markup.CodeBlock ls -> Html.code_ $ unlines ls




