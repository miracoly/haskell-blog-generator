-- HsBlog.hs
module HsBlog (convertSingle, convertDirectory, confirm) where

import HsBlog.Convert (convert)
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

