module Jikka.Main.Subcommand.Debug (run) where

import Data.Text (unpack)
import qualified Data.Text.IO as T (readFile)
import qualified Jikka.CPlusPlus.Convert.FromCore as FromCore
import qualified Jikka.CPlusPlus.Format as FormatCPlusPlus
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert.ANormal as ANormal
import qualified Jikka.Core.Convert.RemoveUnusedVars as RemoveUnusedVars
import qualified Jikka.Core.Convert.StrengthReduction as StrengthReduction
import qualified Jikka.Core.Format as FormatCore
import qualified Jikka.Python.Convert.ToRestrictedPython as ToRestrictedPython
import qualified Jikka.Python.Parse.Alex as PythonLexer
import qualified Jikka.Python.Parse.Happy as PythonParser
import qualified Jikka.RestrictedPython.Convert.ToCore as ToCore
import qualified Jikka.RestrictedPython.Convert.TypeInfer as TypeInfer
import qualified Jikka.RestrictedPython.Format as FormatPython

-- | TODO: remove this
liftEither' :: Either String a -> ExceptT Error IO a
liftEither' f = case f of
  Left msg -> throwError (Error msg)
  Right x -> return x

put :: String -> String -> ExceptT Error IO ()
put title message = do
  liftIO $ putStrLn (title ++ ":")
  let indent = unlines . map ("    " ++) . lines
  liftIO $ putStrLn (indent message)

run :: FilePath -> ExceptT Error IO ()
run path = do
  let counter = 0
  put "path" $ show path
  prog <- liftIO $ T.readFile path
  put "input" $ unpack prog
  prog <- liftEither $ PythonLexer.run (unpack prog)
  put "tokens" $ unlines (map show prog)
  prog <- liftEither $ PythonParser.run prog
  put "parsed" $ show prog
  (prog, counter) <- runAlphaT counter $ ToRestrictedPython.run prog
  put "restricted" . unpack =<< liftEither' (FormatPython.run prog)
  prog <- liftEither' $ TypeInfer.run prog
  put "infered types" . unpack =<< liftEither' (FormatPython.run prog)
  prog <- liftEither' $ ToCore.run prog
  put "core" . unpack =<< liftEither' (FormatCore.run prog)
  prog <- liftEither' $ RemoveUnusedVars.run prog
  put "core simplified" . unpack =<< liftEither' (FormatCore.run prog)
  prog <- liftEither' $ StrengthReduction.run prog
  put "core reduced" . unpack =<< liftEither' (FormatCore.run prog)
  prog <- liftEither' $ ANormal.run prog
  put "simplify for codgen" . unpack =<< liftEither' (FormatCore.run prog)
  (prog, _) <- liftEither' . runAlphaT counter $ FromCore.run prog
  put "generated code" . unpack =<< liftEither' (FormatCPlusPlus.run prog)
  return ()
