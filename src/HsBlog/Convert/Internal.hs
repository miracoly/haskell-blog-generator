-- HsBlog/Convert/Internal.hs

module HsBlog.Convert.Internal (module HsBlog.Convert.Internal) where

import qualified HsBlog.Html.Internal as Html
import qualified HsBlog.Markup.Internal as Markup

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



