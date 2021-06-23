{-# LANGUAGE LambdaCase #-}

module Jikka.Main.Target where

import Jikka.Common.Error

data Target
  = PythonTarget
  | RestrictedPythonTarget
  | CoreTarget
  | CPlusPlusTarget
  deriving (Eq, Ord, Show, Read)

parseTarget :: String -> Either Error Target
parseTarget = \case
  "python" -> return PythonTarget
  "rpython" -> return RestrictedPythonTarget
  "core" -> return CoreTarget
  "cxx" -> return CPlusPlusTarget
  s -> throwCommandLineError $ "invalid target: " ++ s
