-- HsBlog.hs
module HsBlog (convertSingle, confirm) where

import HsBlog.Convert (convert)
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup
import System.IO (Handle, hGetContents, hPutStrLn)
import HsBlog.Env (defaultEnv)

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

confirm :: IO Bool
confirm =
  putStrLn "Are you sure? (y/n)"
    *> getLine >>= \case
      "y" -> pure True
      "n" -> pure False
      _ -> putStrLn "Invalid response. use y or n" *> confirm

process :: Html.Title -> String -> String
process title = Html.render . convert defaultEnv title . Markup.parse

