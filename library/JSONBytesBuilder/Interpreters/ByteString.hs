module JSONBytesBuilder.Interpreters.ByteString
(
  compactJSON,
)
where

import JSONBytesBuilder.Prelude hiding (length, null)
import JSONBytesBuilder.Builder
import qualified JSONBytesBuilder.Interpreters.LazyByteString as A
import qualified Data.ByteString.Lazy as B


-- |
-- Produce a strict JSON ByteString with compact syntax
{-# INLINE compactJSON #-}
compactJSON :: Literal -> ByteString
compactJSON =
  B.toStrict . A.compactJSON
