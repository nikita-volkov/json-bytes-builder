module JSONBytesBuilder.Private.ByteString.Builder
where

import JSONBytesBuilder.Private.Prelude hiding (length, null)
import Data.ByteString.Builder
import qualified JSONBytesBuilder.Private.ByteString.BoundedPrim as A
import qualified Data.ByteString.Builder.Prim as A
import qualified Data.Text.Encoding as B
import qualified Data.Scientific as C
import qualified Data.ByteString.Builder.Scientific as D


{-# INLINABLE null #-}
null :: Builder
null =
  A.primBounded A.null ()

{-# INLINABLE boolean #-}
boolean :: Bool -> Builder
boolean =
  A.primBounded A.boolean

{-# INLINABLE string #-}
string :: Text -> Builder
string x =
  char8 '"' <> B.encodeUtf8BuilderEscaped A.stringEncodedByte x <> char8 '"'

{-# INLINABLE scientific #-}
scientific :: Scientific -> Builder
scientific n =
  if e < 0
    then D.scientificBuilder n
    else integerDec (C.coefficient n * 10 ^ e)
  where
    e =
      C.base10Exponent n

{-# INLINABLE inCurlies #-}
inCurlies :: Builder -> Builder
inCurlies x =
  char8 '{' <> x <> char8 '}'

{-# INLINABLE row #-}
row :: Text -> Builder -> Builder
row key value =
  string key <> char8 ':' <> value

{-# INLINABLE inSquarelies #-}
inSquarelies :: Builder -> Builder
inSquarelies x =
  char8 '[' <> x <> char8 ']'

{-# INLINABLE commaSeparated #-}
commaSeparated :: Builder -> Builder -> Builder
commaSeparated left right =
  left <> char8 ',' <> right

{-# INLINABLE emptyObject #-}
emptyObject :: Builder
emptyObject =
  A.primBounded A.emptyObject ()

{-# INLINABLE emptyArray #-}
emptyArray :: Builder
emptyArray =
  A.primBounded A.emptyArray ()
