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
