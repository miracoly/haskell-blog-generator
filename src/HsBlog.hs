-- HsBlog.hs

module HsBlog (main, process) where

import HsBlog.Convert (convert)
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup
import System.Directory (doesFileExist)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= dispatch
  where
    dispatch :: [String] -> IO ()
    dispatch args =
      case args of
        [] -> getContents >>= putStrLn . process "Empty title"
        [input, output] -> readWriteFile input output
        _ -> putStrLn "Usage: runghc Main.hs [-- <input-file> <output-file>]"

    readWriteFile :: String -> String -> IO ()
    readWriteFile input output =
      readFile input
        >>= \content ->
          doesFileExist output
            >>= \doesExist ->
              let writeResult = writeFile output (process "Empty title" content)
               in if doesExist then askForConfirmation writeResult else writeResult

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse

askForConfirmation :: IO () -> IO ()
askForConfirmation io =
  putStrLn "Output file already exists. Do you want to overwrite? (y/n)"
    *> getLine >>= \answer -> if answer == "y" then io else pure ()
