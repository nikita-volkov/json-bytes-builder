module JSONBuilder.Interpreters.ByteString
(
  compactJSON,
)
where

import JSONBuilder.Prelude hiding (length, null)
import JSONBuilder.Model
import qualified JSONBuilder.Interpreters.LazyByteString as A
import qualified Data.ByteString.Lazy as B


-- |
-- Produce a strict JSON ByteString with compact syntax
{-# INLINE compactJSON #-}
compactJSON :: JSON -> ByteString
compactJSON =
  B.toStrict . A.compactJSON
