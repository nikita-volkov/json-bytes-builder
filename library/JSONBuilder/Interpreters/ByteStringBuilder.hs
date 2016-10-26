module JSONBuilder.Interpreters.ByteStringBuilder
(
  compactJSON,
)
where

import JSONBuilder.Prelude hiding (length, null)
import JSONBuilder.Model
import Data.ByteString.Builder
import qualified JSONBuilder.Interpreters.ByteStringBuilder.Prim as A
import qualified Data.ByteString.Builder.Prim as A
import qualified Data.Text.Encoding as B
import qualified Data.Scientific as C
import qualified Data.ByteString.Builder.Scientific as D


-- |
-- Produce a JSON ByteString Builder with compact syntax
compactJSON :: JSON -> Builder
compactJSON =
  \case
    JSON_String x -> string x
    JSON_Number x -> number x
    JSON_Boolean x -> boolean x
    JSON_Object x -> compactObject x
    JSON_Array x -> compactArray x
    JSON_Null -> null

null :: Builder
null =
  A.primBounded A.null ()

boolean :: Bool -> Builder
boolean =
  A.primBounded A.boolean

string :: Text -> Builder
string x =
  char8 '"' <> B.encodeUtf8BuilderEscaped A.stringEncodedByte x <> char8 '"'

number :: Number -> Builder
number =
  \case
    Number_Int x -> intDec x
    Number_Double x -> doubleDec x
    Number_Scientific x -> scientific x

scientific :: Scientific -> Builder
scientific n =
  if e < 0
    then D.scientificBuilder n
    else integerDec (C.coefficient n * 10 ^ e)
  where
    e =
      C.base10Exponent n

compactObject :: Object -> Builder
compactObject x =
  char8 '{' <> compactObjectBody x <> char8 '}'

compactObjectBody :: Object -> Builder
compactObjectBody =
  \case
    Object_Row key value -> string key <> char8 ':' <> compactJSON value
    Object_Append left right -> compactObjectBody left <> char8 ',' <> compactObjectBody right
    Object_Empty -> mempty

compactArray :: Array -> Builder
compactArray x =
  char8 '[' <> compactArrayBody x <> char8 ']'

compactArrayBody :: Array -> Builder
compactArrayBody =
  \case
    Array_Element value -> compactJSON value
    Array_Append left right -> compactArrayBody left <> char8 ',' <> compactArrayBody right
    Array_Empty -> mempty
