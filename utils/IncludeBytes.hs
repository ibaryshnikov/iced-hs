{-# LANGUAGE TemplateHaskell #-}

-- example of embedding data from files at compile time

module IncludeBytes (includeBytes) where

import Control.Monad
import Data.ByteString qualified as ByteString
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

includeBytes :: FilePath -> Q Exp
includeBytes path = do
  string <- runIO (ByteString.readFile path)
  addDependentFile path
  let bytes = ByteString.unpack string
  [|bytes|]
