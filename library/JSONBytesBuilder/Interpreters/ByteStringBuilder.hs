module JSONBytesBuilder.Interpreters.ByteStringBuilder
(
  jsonLiteral,
)
where

import JSONBytesBuilder.Prelude hiding (length, null)
import JSONBytesBuilder.Builder
import Data.ByteString.Builder


-- |
-- Produce a JSON ByteString builder with compact syntax from a literal builder.
{-# INLINE jsonLiteral #-}
jsonLiteral :: Literal -> Builder
jsonLiteral =
  unsafeCoerce
