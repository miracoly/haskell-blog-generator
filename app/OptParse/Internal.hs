-- OptParse/Internal.hs
module OptParse.Internal (module OptParse.Internal) where

import Data.Maybe (fromMaybe)
import HsBlog.Env (Env (..), defaultEnv, eBlogName, eStylesheetPath)
import Options.Applicative

data Options
  = ConvertSingle SingleInput SingleOutput AllowOverwrite
  | ConvertDir FilePath FilePath Env
  deriving (Show)

parse :: IO Options
parse = execParser opts

opts :: ParserInfo Options
opts =
  info
    (helper <*> pOptions)
    ( fullDesc
        <> header "hs-blog-gen"
        <> progDesc "Convert markup files or directories to html"
    )

pOptions :: Parser Options
pOptions = subparser $ pConvertSingleCommand <> pConvertDirCommand

type AllowOverwrite = Bool

pAllowOverwrite :: Parser AllowOverwrite
pAllowOverwrite = parser
  where
    parser =
      flag
        False
        True
        ( long "replace"
            <> short 'r'
            <> help "Allow overwrite if output file already exists"
        )

-- * Directory conversion parser

pConvertDirCommand :: Mod CommandFields Options
pConvertDirCommand =
  command "convert-dir" pConvertDirInfo

pConvertDirInfo :: ParserInfo Options
pConvertDirInfo =
  info
    (helper <*> pConvertDir)
    (progDesc "Convert a whole directory with markup files to html")

pConvertDir :: Parser Options
pConvertDir = ConvertDir <$> pInputDir <*> pOutputDir <*> pEnv

pEnv :: Parser Env
pEnv = Env <$> pBlogName <*> pStylesheet

pInputDir :: Parser FilePath
pInputDir =
  strOption
    ( long "input"
        <> short 'i'
        <> metavar "DIRECORY"
        <> help "Input directory"
    )

pOutputDir :: Parser FilePath
pOutputDir =
  strOption
    ( long "output"
        <> short 'o'
        <> metavar "DIRECTORY"
        <> help "Output directory"
    )

pBlogName :: Parser String
pBlogName =
  strOption
    ( long "name"
        <> short 'N'
        <> metavar "STRING"
        <> help "Blog name"
        <> value (eBlogName defaultEnv)
        <> showDefault
    )

pStylesheet :: Parser String
pStylesheet =
  strOption
    ( long "style"
        <> short 'S'
        <> metavar "DIRECTORY"
        <> help "Stylesheet filename"
        <> value (eStylesheetPath defaultEnv)
        <> showDefault
    )

-- * Single File parser

data SingleInput
  = Stdin
  | InputFile FilePath
  deriving (Show)

data SingleOutput
  = Stdout
  | OutputFile FilePath
  deriving (Show)

pConvertSingleCommand :: Mod CommandFields Options
pConvertSingleCommand =
  command "convert" pConvertSingleInfo

pConvertSingleInfo :: ParserInfo Options
pConvertSingleInfo =
  info
    (helper <*> pConvertSingle)
    (progDesc "Convert a single markup source to html")

pConvertSingle :: Parser Options
pConvertSingle = ConvertSingle <$> pSingleInput <*> pSingleOutput <*> pAllowOverwrite

pSingleInput :: Parser SingleInput
pSingleInput = fromMaybe Stdin <$> optional pInputFile

pSingleOutput :: Parser SingleOutput
pSingleOutput = fromMaybe Stdout <$> optional pOutputFile

pInputFile :: Parser SingleInput
pInputFile = InputFile <$> parser
  where
    parser =
      strOption
        ( long "input"
            <> short 'i'
            <> metavar "FILE"
            <> help "Input File"
        )

pOutputFile :: Parser SingleOutput
pOutputFile = fmap OutputFile parser
  where
    parser =
      strOption
        ( long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "Output File"
        )
