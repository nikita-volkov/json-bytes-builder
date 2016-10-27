module JSONBytesBuilder.Interpreters.LazyByteString
(
  compactJSON,
)
where

import JSONBytesBuilder.Prelude hiding (length, null)
import JSONBytesBuilder.Builder
import Data.ByteString.Builder
import qualified JSONBytesBuilder.Interpreters.ByteStringBuilder as A
import qualified Data.ByteString.Lazy as B


-- |
-- Produce a lazy JSON ByteString with compact syntax
{-# INLINE compactJSON #-}
compactJSON :: Literal -> B.ByteString
compactJSON =
  toLazyByteString . A.compactJSON
