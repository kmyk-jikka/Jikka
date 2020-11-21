module Jikka.Subcommand.Execute (run) where

import Data.Text (Text)
import qualified Jikka.Converter.Optimizer as Opt
import qualified Jikka.Converter.Python.Alpha as ConvertAlpha
import qualified Jikka.Converter.Python.Convert as FromParsed
import qualified Jikka.Deserializer.Python as FromPython

run :: FilePath -> Text -> IO (Either String Text)
run path input = do
  let prog = do
        prog <- FromPython.run path input
        prog <- ConvertAlpha.run prog
        FromParsed.run prog
  putStrLn "Hello, world!"
  return $ Left "not implemented yet"
