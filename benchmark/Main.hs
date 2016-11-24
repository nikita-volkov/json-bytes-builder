module Main where

import Rebase.Prelude
import Criterion.Main
import qualified Main.Model
import qualified Main.JSONBytesBuilder
import qualified Main.Aeson
import qualified Data.Aeson
import qualified Rebase.Data.ByteString.Lazy
import qualified JSONBytesBuilder.ByteString.ByteString
import qualified JSONBytesBuilder.ByteString.LazyByteString

main =
  do
    input <- load "samples/twitter100.json"
    defaultMain
      [
        bench "strict" (nf (JSONBytesBuilder.ByteString.ByteString.jsonLiteral . Main.JSONBytesBuilder.result) input)
        ,
        bench "lazy" (nf (JSONBytesBuilder.ByteString.LazyByteString.jsonLiteral . Main.JSONBytesBuilder.result) input)
      ]

load :: FilePath -> IO Main.Model.Result
load fileName =
  (=<<) (either fail return . Data.Aeson.eitherDecode') $
  Rebase.Data.ByteString.Lazy.readFile fileName
