module JSONBytesBuilder.Builder
(
  JSON,
  null,
  boolean,
  number_int,
  number_integer,
  number_double,
  number_scientific,
  string,
  object,
  array,
  Object,
  row,
  Array,
  element,
)
where

import JSONBytesBuilder.Prelude hiding (null)
import qualified Data.ByteString.Builder as A
import qualified JSONBytesBuilder.ByteString.Builders as E


newtype JSON =
  JSON A.Builder

newtype Object =
  Object (Maybe A.Builder)

instance Monoid Object where
  {-# INLINE mempty #-}
  mempty =
    Object Nothing
  {-# INLINE mappend #-}
  mappend =
    \case
      Object (Just left) ->
        \case
          Object (Just right) ->
            Object (Just (left <> A.char8 ',' <> right))
          _ ->
            Object (Just left)
      Object Nothing ->
        id

newtype Array =
  Array (Maybe A.Builder)

instance Monoid Array where
  {-# INLINE mempty #-}
  mempty =
    Array Nothing
  {-# INLINE mappend #-}
  mappend =
    \case
      Array (Just left) ->
        \case
          Array (Just right) ->
            Array (Just (left <> A.char8 ',' <> right))
          _ ->
            Array (Just left)
      Array Nothing ->
        id

{-# INLINE null #-}
null :: JSON
null =
  JSON (inline E.null)

{-# INLINE boolean #-}
boolean :: Bool -> JSON
boolean =
  JSON . inline E.boolean

{-# INLINE number_int #-}
number_int :: Int -> JSON
number_int =
  JSON . inline A.intDec

{-# INLINE number_integer #-}
number_integer :: Integer -> JSON
number_integer =
  JSON . inline A.integerDec

{-# INLINE number_double #-}
number_double :: Double -> JSON
number_double =
  JSON . inline A.doubleDec

{-# INLINE number_scientific #-}
number_scientific :: Scientific -> JSON
number_scientific =
  JSON . inline E.scientific

{-# INLINE string #-}
string :: Text -> JSON
string =
  JSON . inline E.string

{-# INLINE object #-}
object :: Object -> JSON
object (Object x) =
  JSON (maybe E.emptyObject (inline E.inCurlies) x)

{-# INLINE array #-}
array :: Array -> JSON
array (Array x) =
  JSON (maybe E.emptyArray (inline E.inCurlies) x)

{-# INLINE row #-}
row :: Text -> JSON -> Object
row key (JSON value) =
  Object (Just (inline E.row key value))

{-# INLINE element #-}
element :: JSON -> Array
element (JSON value) =
  Array (Just value)

