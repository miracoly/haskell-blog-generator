-- HsBlog/Convert/Internal.hs

module HsBlog.Convert.Internal (module HsBlog.Convert.Internal) where

import HsBlog.Env
import qualified HsBlog.Html.Internal as Html
import qualified HsBlog.Markup.Internal as Markup

convert :: Env -> Html.Title -> Markup.Document -> Html.Html
convert env title doc =
  let htmlHead =
        Html.title_ (eBlogName env <> " - " <> title)
          <> Html.stylesheet_ (eStylesheetPath env)
      article = foldMap convertStructure doc
      websiteTitle = Html.h_ 1 $ Html.link_ "index.html" $ Html.txt_ $ eBlogName env
      body = websiteTitle <> article
   in Html.html_ htmlHead body

convertStructure :: Markup.Structure -> Html.Structure
convertStructure struct =
  case struct of
    Markup.Heading n txt -> Html.h_ n $ Html.txt_ txt
    Markup.Paragraph p -> Html.p_ $ Html.txt_ p
    Markup.UnorderedList ls -> Html.ul_ $ map (Html.p_ . Html.txt_) ls
    Markup.OrderedList ls -> Html.ol_ $ map (Html.p_ . Html.txt_) ls
    Markup.CodeBlock ls -> Html.code_ $ unlines ls
