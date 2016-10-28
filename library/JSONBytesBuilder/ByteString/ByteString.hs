module JSONBytesBuilder.ByteString.ByteString
(
  jsonLiteral,
)
where

import JSONBytesBuilder.Private.Prelude hiding (length, null)
import JSONBytesBuilder.Builder
import qualified JSONBytesBuilder.ByteString.LazyByteString as A
import qualified Data.ByteString.Lazy as B


-- |
-- Produce a strict JSON ByteString with compact syntax from a literal builder.
{-# INLINE jsonLiteral #-}
jsonLiteral :: Literal -> ByteString
jsonLiteral =
  B.toStrict . A.jsonLiteral
