{-# LANGUAGE LambdaCase #-}

module Jikka.Common.Name where

import Data.List
import Data.Maybe
import Text.Read

type OccName = Maybe String

type NameFlavour = Maybe Int

toFlavouredName :: String -> (OccName, NameFlavour)
toFlavouredName = \case
  "_" -> (Nothing, Nothing)
  s -> case elemIndex '$' s of
    Nothing -> (Just s, Nothing)
    Just i ->
      let (occ, flavour) = splitAt i s
          occ' = if occ == "" then Nothing else Just occ
          flavour' = case readMaybe (tail flavour) of
            Nothing -> error $ "Jikka.Common.Name.toFlavouredName: invalid flavoured name: " ++ s
            Just i -> i
       in (occ', Just flavour')

formatFlavouredName :: OccName -> NameFlavour -> String
formatFlavouredName occ flavour = case (occ, flavour) of
  (Nothing, Nothing) -> "_"
  _ -> fromMaybe "" occ ++ maybe "" (('$' :) . show) flavour
