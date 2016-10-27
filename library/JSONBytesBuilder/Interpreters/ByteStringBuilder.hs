module JSONBytesBuilder.Interpreters.ByteStringBuilder
(
  compactJSON,
)
where

import JSONBytesBuilder.Prelude hiding (length, null)
import JSONBytesBuilder.Builder
import Data.ByteString.Builder


-- |
-- Produce a JSON ByteString builder with compact syntax
{-# INLINE compactJSON #-}
compactJSON :: JSON -> Builder
compactJSON =
  unsafeCoerce
