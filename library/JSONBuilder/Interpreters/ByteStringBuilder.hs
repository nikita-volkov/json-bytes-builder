module JSONBuilder.Interpreters.ByteStringBuilder
(
  compactJSON,
)
where

import JSONBuilder.Prelude hiding (length, null)
import JSONBuilder.Builder
import Data.ByteString.Builder


-- |
-- Produce a JSON ByteString builder with compact syntax
{-# INLINE compactJSON #-}
compactJSON :: JSON -> Builder
compactJSON =
  unsafeCoerce
