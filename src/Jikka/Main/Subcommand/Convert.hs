module Jikka.Main.Subcommand.Convert (run) where

import Data.Text (Text)
import qualified Jikka.CPlusPlus.Convert.FromCore as FromCore
import qualified Jikka.CPlusPlus.Format as FormatCPlusPlus
import Jikka.Common.Alpha
import Jikka.Common.Error
import qualified Jikka.Core.Convert.ANormal as ANormal
import qualified Jikka.Core.Convert.Optimize as Optimize
import qualified Jikka.Python.Convert.ToRestrictedPython as ToRestrictedPython
import qualified Jikka.Python.Parse as FromPython
import qualified Jikka.RestrictedPython.Convert.ToCore as ToCore
import qualified Jikka.RestrictedPython.Convert.TypeInfer as TypeInfer

-- | TODO: remove this
lift' :: Either String a -> Either Error a
lift' f = case f of
  Left msg -> throwError (Error msg)
  Right x -> return x

run :: FilePath -> Text -> Either Error Text
run path input = do
  let counter = 0
  prog <- FromPython.run path input
  (prog, counter) <- runAlphaT counter $ ToRestrictedPython.run prog
  prog <- lift' $ TypeInfer.run prog
  prog <- lift' $ ToCore.run prog
  prog <- lift' $ Optimize.run prog
  prog <- lift' $ ANormal.run prog
  (prog, _) <- lift' . runAlphaT counter $ FromCore.run prog
  lift' $ FormatCPlusPlus.run prog
