module Markup.Internal (module Markup.Internal) where

import Numeric.Natural (Natural)

type Document = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Show)
  
parse :: String -> Document
parse = parseLines [] . lines

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph txts =
  let paragraph = Paragraph (unlines (reverse currentParagraph))
   in case txts of
        [] -> [paragraph]
        ln : lns ->
          if trim ln == ""
            then paragraph : parseLines [] lns
            else parseLines (ln : currentParagraph) lns

trim :: String -> String
trim = unwords . words
