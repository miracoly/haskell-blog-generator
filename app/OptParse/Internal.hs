-- OptParse/Internal.hs
module OptParse.Internal (module OptParse.Internal) where

import Options.Applicative
import Data.Maybe (fromMaybe)

data Options
  = ConvertSingle SingleInput SingleOutput
  | ConvertDir FilePath FilePath
  deriving (Show)

data SingleInput
  = Stdin
  | InputFile FilePath
  deriving (Show)

data SingleOutput
  = Stdout
  | OutputFile FilePath
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

pConvertSingleCommand :: Mod CommandFields Options
pConvertSingleCommand =
  command "convert" pConvertSingleInfo

pConvertDirCommand :: Mod CommandFields Options
pConvertDirCommand =
  command "convert-dir" pConvertDirInfo

pConvertSingleInfo :: ParserInfo Options
pConvertSingleInfo =
  info
    (helper <*> pConvertSingle)
    (progDesc "Convert a single markup source to html")

pConvertDirInfo :: ParserInfo Options
pConvertDirInfo =
  info
    (helper <*> pConvertDir)
    (progDesc "Convert a whole directory with markup files to html")

pConvertDir :: Parser Options
pConvertDir = ConvertDir <$> pInputDir <*> pOutputDir

pConvertSingle :: Parser Options
pConvertSingle = ConvertSingle <$> pSingleInput <*> pSingleOutput

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
