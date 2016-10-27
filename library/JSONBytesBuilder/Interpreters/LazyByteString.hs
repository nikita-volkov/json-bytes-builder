module JSONBytesBuilder.Interpreters.LazyByteString
(
  jsonLiteral,
)
where

import JSONBytesBuilder.Prelude hiding (length, null)
import JSONBytesBuilder.Builder
import Data.ByteString.Builder
import qualified JSONBytesBuilder.Interpreters.ByteStringBuilder as A
import qualified Data.ByteString.Lazy as B


-- |
-- Produce a lazy JSON ByteString with compact syntax from a literal builder.
{-# INLINE jsonLiteral #-}
jsonLiteral :: Literal -> B.ByteString
jsonLiteral =
  toLazyByteString . A.jsonLiteral
