module JSONBytesBuilder.Private.ByteString.Builder
where

import JSONBytesBuilder.Private.Prelude hiding (length, null)
import Data.ByteString.Builder
import qualified JSONBytesBuilder.Private.ByteString.BoundedPrim as A
import qualified Data.ByteString.Builder.Prim as A
import qualified Data.ByteString.Builder.Scientific as D
import qualified Data.ByteString as E
import qualified Data.ByteString.Lazy as F
import qualified Data.Text.Encoding as B
import qualified Data.Text.Lazy as G
import qualified Data.Text.Lazy.Encoding as H
import qualified Data.Scientific as C


{-# INLINABLE null #-}
null :: Builder
null =
  A.primBounded A.null ()

{-# INLINABLE boolean #-}
boolean :: Bool -> Builder
boolean =
  A.primBounded A.boolean

{-# INLINABLE stringFromText #-}
stringFromText :: Text -> Builder
stringFromText x =
  char8 '"' <> B.encodeUtf8BuilderEscaped A.stringEncodedByte x <> char8 '"'

{-# INLINABLE stringFromLazyText #-}
stringFromLazyText :: G.Text -> Builder
stringFromLazyText x =
  char8 '"' <> H.encodeUtf8BuilderEscaped A.stringEncodedByte x <> char8 '"'

{-# INLINABLE stringFromBytes #-}
stringFromBytes :: ByteString -> Builder
stringFromBytes x =
  char8 '"' <> A.primMapByteStringBounded (inline A.stringEncodedByte) x <> char8 '"'

{-# INLINABLE stringFromLazyBytes #-}
stringFromLazyBytes :: F.ByteString -> Builder
stringFromLazyBytes x =
  char8 '"' <> A.primMapLazyByteStringBounded (inline A.stringEncodedByte) x <> char8 '"'

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
  stringFromText key <> char8 ':' <> value

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
