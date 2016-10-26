module JSONBuilder.Interpreters.LazyByteString
(
  compactJSON,
)
where

import JSONBuilder.Prelude hiding (length, null)
import JSONBuilder.Model
import Data.ByteString.Builder
import qualified JSONBuilder.Interpreters.ByteStringBuilder as A
import qualified Data.ByteString.Lazy as B


-- |
-- Produce a lazy JSON ByteString with compact syntax
{-# INLINE compactJSON #-}
compactJSON :: JSON -> B.ByteString
compactJSON =
  toLazyByteString . A.compactJSON
