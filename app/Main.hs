-- app/Main.hs
import qualified HsBlog
import OptParse
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO

main :: IO ()
main = do
  options <- parse
  case options of
    ConvertDir input output -> HsBlog.convertDirectory input output
    ConvertSingle input output allowOverwrite -> do
      (title, inputHandle) <-
        case input of
          Stdin -> pure ("", stdin)
          InputFile file -> (,) file <$> openFile file ReadMode
      outputHandle <-
        case output of
          Stdout -> pure stdout
          OutputFile file -> do
            exists <- doesFileExist file
            shouldOpenFile <-
              if exists && not allowOverwrite
                then HsBlog.confirm
                else pure True
            if shouldOpenFile
              then openFile file WriteMode
              else exitFailure
      HsBlog.convertSingle title inputHandle outputHandle
      hClose inputHandle
      hClose outputHandle
