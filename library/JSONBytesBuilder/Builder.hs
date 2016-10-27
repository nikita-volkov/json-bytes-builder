module JSONBytesBuilder.Builder
(
  JSON,
  null,
  boolean,
  number_int,
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
import qualified JSONBytesBuilder.Builders as E


newtype JSON =
  JSON A.Builder

newtype Object =
  Object (Maybe A.Builder)

instance Monoid Object where
  mempty =
    Object Nothing
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
  mempty =
    Array Nothing
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
  JSON E.null

{-# INLINE boolean #-}
boolean :: Bool -> JSON
boolean =
  JSON . E.boolean

{-# INLINE number_int #-}
number_int :: Int -> JSON
number_int =
  JSON . A.intDec

{-# INLINE number_double #-}
number_double :: Double -> JSON
number_double =
  JSON . A.doubleDec

{-# INLINE number_scientific #-}
number_scientific :: Scientific -> JSON
number_scientific =
  JSON . E.scientific

{-# INLINE string #-}
string :: Text -> JSON
string =
  JSON . E.string

{-# INLINE object #-}
object :: Object -> JSON
object (Object x) =
  JSON (E.inCurlies (fold x))

{-# INLINE array #-}
array :: Array -> JSON
array (Array x) =
  JSON (E.inCurlies (fold x))

{-# INLINE row #-}
row :: Text -> JSON -> Object
row key (JSON value) =
  Object (Just (E.row key value))

{-# INLINE element #-}
element :: JSON -> Array
element (JSON value) =
  Array (Just value)

