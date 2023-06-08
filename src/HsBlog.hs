-- HsBlog.hs
module HsBlog (convertSingle, convertDirectory, confirm) where

import HsBlog.Convert (convert, convertStructure)
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup
import System.IO (Handle, hGetContents, hPutStrLn)

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory = undefined

confirm :: IO Bool
confirm =
  putStrLn "Are you sure? (y/n)"
    *> getLine >>= \case
      "y" -> pure True
      "n" -> pure False
      _ -> putStrLn "Invalid response. use y or n" *> confirm

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex =
  Html.html_ "Blog" . (<>) headline . mconcat . map singleIndex
  where
    headline :: Html.Structure
    headline = Html.h_ 1 (Html.link_ "index.html" (Html.txt_ "Blog")) <> Html.h_ 2 (Html.txt_ "Posts")
    singleIndex :: (FilePath, Markup.Document) -> Html.Structure
    singleIndex (file, doc) =
      case doc of
        (Markup.Heading 1 heading) : article ->
          Html.h_ 3 (Html.link_ file (Html.txt_ heading))
            <> foldMap convertStructure (take 3 article)
            <> Html.p_ (Html.link_ file (Html.txt_ "..."))
        _ ->
          Html.h_ 3 (Html.link_ file (Html.txt_ file))
