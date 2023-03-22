-- Main.hs

import Data.Maybe (listToMaybe)
import Data.Word (Word8)
import Html
import Markup

main :: IO ()
main = putStr $ render myHtml
--  txt <- readFile "resources/sample.txt"
--  putStrLn $ if parse txt == example4 then "CORRECT" else "FALSE"

example4 :: [Markup.Structure]
example4 =
  [ Heading 1 "Compiling programs with ghc",
    Paragraph "Running ghc invokes the Glasgow Haskell Compiler (GHC), and can be used to compile Haskell modules and programs into native executables and libraries.",
    Paragraph "Create a new Haskell source file named hello.hs, and write the following code in it:",
    CodeBlock
      [ "main = putStrLn \"Hello, Haskell!\""
      ],
    Paragraph "Now, we can compile the program by invoking ghc with the file name:",
    CodeBlock
      [ "➜ ghc hello.hs",
        "[1 of 1] Compiling Main             ( hello.hs, hello.o )",
        "Linking hello ..."
      ],
    Paragraph "GHC created the following files:",
    UnorderedList
      [ "hello.hi - Haskell interface file",
        "hello.o - Object file, the output of the compiler before linking",
        "hello (or hello.exe on Microsoft Windows) - A native runnable executable."
      ],
    Paragraph "GHC will produce an executable when the source file satisfies both conditions:",
    OrderedList
      [ "Defines the main function in the source file",
        "Defines the module name to be Main, or does not have a module declaration"
      ],
    Paragraph "Otherwise, it will only produce the .o and .hi files."
  ]

myHtml :: Html
myHtml =
  html_
    "My title"
    $ (<>)
      (h_ 1 "My headline")
      $ (<>)
        (p_ "Let's learn about haskell")
        $ (<>)
          ( ul_
              [ p_ "item 1",
                p_ "item 2",
                p_ "item 3"
              ]
          )
          $ (<>)
            ( ol_
                [ p_ "item 1",
                  p_ "item 2",
                  p_ "item 3"
                ]
            )
            (code_ "This is a code block")

one :: Document
one = [Paragraph "Hello, world!"]

two :: Document
two =
  [ Heading 1 "Welcome",
    Paragraph "To this tutorial about Haskell."
  ]

three :: Document
three =
  [ Paragraph "Remember that multiple lines with no separate are grouped together to a single paragraph but list items remain separate.",
    OrderedList
      [ "Item 1 of a list",
        "Item 2 of the same list"
      ]
  ]

four :: Document
four =
  [ Heading 1 "Compiling programs with ghc",
    Paragraph "Running ghc invokes the Glasgow Haskell Compiler (GHC), and can be used to compile Haskell modules and programs into native executables and libraries.",
    Paragraph "Create a new Haskell source file named hello.hs, and write the following code in it:",
    CodeBlock ["main = putStrLn \"Hello, Haskell!\""],
    Paragraph "Now, we can compile the program by invoking ghc with the file name:",
    CodeBlock
      [ "➜ ghc hello.hs",
        "[1 of 1] Compiling Main             ( hello.hs, hello.o )",
        "Linking hello ..."
      ],
    Paragraph "GHC created the following files:",
    UnorderedList
      [ "hello.hi - Haskell interface file",
        "hello.o - Object file, the output of the compiler before linking",
        "hello (or hello.exe on Microsoft Windows) - A native runnable executable."
      ],
    Paragraph "GHC will produce an executable when the source file satisfies both conditions:",
    OrderedList
      [ "Defines the main function in the source file",
        "Defines the module name to be Main, or does not have a module declaration"
      ],
    Paragraph "Otherwise, it will only produce the .o and .hi files."
  ]

data Brightness
  = Dark
  | Bright

data EightColor
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

data AnsiColor
  = AnsiColor Brightness EightColor

data Color
  = RGB Word8 Word8 Word8

ansiToUbuntu :: AnsiColor -> Color
ansiToUbuntu (AnsiColor brightness color) =
  case brightness of
    Dark ->
      case color of
        Black -> RGB 0 0 0
        Red -> RGB 194 54 33
        Green -> RGB 37 188 36
        Yellow -> RGB 173 173 39
        Blue -> RGB 73 46 225
        Magenta -> RGB 211 56 211
        Cyan -> RGB 51 187 200
        White -> RGB 203 204 205
    Bright ->
      case color of
        Black -> RGB 129 131 131
        Red -> RGB 252 57 31
        Green -> RGB 49 231 34
        Yellow -> RGB 234 236 35
        Blue -> RGB 88 51 255
        Magenta -> RGB 249 53 248
        Cyan -> RGB 20 240 240
        White -> RGB 233 235 235

isBright :: AnsiColor -> Bool
isBright ansiColor =
  case ansiColor of
    AnsiColor Bright _ -> True
    _ -> False

isEmpty :: [a] -> Bool
isEmpty ls =
  case listToMaybe ls of
    Nothing -> True
    Just _ -> False

isEmpty_ :: [a] -> Bool
isEmpty_ ls =
  case ls of
    [] -> True
    _ -> False
