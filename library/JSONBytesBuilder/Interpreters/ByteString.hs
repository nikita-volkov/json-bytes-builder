module JSONBytesBuilder.Interpreters.ByteString
(
  jsonLiteral,
)
where

import JSONBytesBuilder.Prelude hiding (length, null)
import JSONBytesBuilder.Builder
import qualified JSONBytesBuilder.Interpreters.LazyByteString as A
import qualified Data.ByteString.Lazy as B


-- |
-- Produce a strict JSON ByteString with compact syntax from a literal builder.
{-# INLINE jsonLiteral #-}
jsonLiteral :: Literal -> ByteString
jsonLiteral =
  B.toStrict . A.jsonLiteral
