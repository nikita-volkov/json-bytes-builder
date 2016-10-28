module JSONBytesBuilder.ByteString.Builder
(
  jsonLiteral,
)
where

import JSONBytesBuilder.Private.Prelude hiding (length, null)
import JSONBytesBuilder.Private.Builder
import Data.ByteString.Builder


-- |
-- Produce a JSON ByteString builder with compact syntax from a literal builder.
{-# INLINE jsonLiteral #-}
jsonLiteral :: Literal -> Builder
jsonLiteral (Literal builder) =
  builder
