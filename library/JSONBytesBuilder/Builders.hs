module JSONBytesBuilder.Builders
where

import JSONBytesBuilder.Prelude hiding (length, null)
import Data.ByteString.Builder
import qualified JSONBytesBuilder.BoundedPrims as A
import qualified Data.ByteString.Builder.Prim as A
import qualified Data.Text.Encoding as B
import qualified Data.Scientific as C
import qualified Data.ByteString.Builder.Scientific as D


{-# INLINE null #-}
null :: Builder
null =
  A.primBounded A.null ()

{-# INLINE boolean #-}
boolean :: Bool -> Builder
boolean =
  A.primBounded A.boolean

{-# INLINE string #-}
string :: Text -> Builder
string x =
  char8 '"' <> B.encodeUtf8BuilderEscaped A.stringEncodedByte x <> char8 '"'

{-# INLINE scientific #-}
scientific :: Scientific -> Builder
scientific n =
  if e < 0
    then D.scientificBuilder n
    else integerDec (C.coefficient n * 10 ^ e)
  where
    e =
      C.base10Exponent n

{-# INLINE inCurlies #-}
inCurlies :: Builder -> Builder
inCurlies x =
  char8 '{' <> x <> char8 '}'

{-# INLINE row #-}
row :: Text -> Builder -> Builder
row key value =
  string key <> char8 ':' <> value

{-# INLINE inSquarlies #-}
inSquarlies :: Builder -> Builder
inSquarlies x =
  char8 '[' <> x <> char8 ']'
