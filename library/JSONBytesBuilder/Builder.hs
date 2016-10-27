-- |
-- DSL for construction of JSON.
module JSONBytesBuilder.Builder
(
  -- ** JSON builders
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
  -- ** Object builders
  Object,
  row,
  -- ** Array builders
  Array,
  element,
)
where

import JSONBytesBuilder.Prelude hiding (null)
import qualified Data.ByteString.Builder as A
import qualified JSONBytesBuilder.ByteString.Builders as E


-- |
-- Builder of any JSON value.
newtype JSON =
  JSON A.Builder

-- |
-- Builder of a JSON Object value.
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

-- |
-- Builder of a JSON Array value.
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

-- |
-- JSON Null literal.
{-# INLINE null #-}
null :: JSON
null =
  JSON (inline E.null)

-- |
-- JSON Boolean literal from 'Bool'.
{-# INLINE boolean #-}
boolean :: Bool -> JSON
boolean =
  JSON . inline E.boolean

-- |
-- JSON Number literal from 'Int'.
{-# INLINE number_int #-}
number_int :: Int -> JSON
number_int =
  JSON . inline A.intDec

-- |
-- JSON Number literal from 'Integer'.
{-# INLINE number_integer #-}
number_integer :: Integer -> JSON
number_integer =
  JSON . inline A.integerDec

-- |
-- JSON Number literal from 'Double'.
{-# INLINE number_double #-}
number_double :: Double -> JSON
number_double =
  JSON . inline A.doubleDec

-- |
-- JSON Number literal from 'Scientific'.
{-# INLINE number_scientific #-}
number_scientific :: Scientific -> JSON
number_scientific =
  JSON . inline E.scientific

-- |
-- JSON String literal from 'Text'.
{-# INLINE string #-}
string :: Text -> JSON
string =
  JSON . inline E.string

-- |
-- JSON Object literal from the 'Object' builder.
{-# INLINE object #-}
object :: Object -> JSON
object (Object x) =
  JSON (maybe E.emptyObject (inline E.inCurlies) x)

-- |
-- JSON Array literal from the 'Array' builder.
{-# INLINE array #-}
array :: Array -> JSON
array (Array x) =
  JSON (maybe E.emptyArray (inline E.inSquarelies) x)

-- |
-- Object builder from a key-value pair,
-- where value is an already encoded JSON literal.
-- 
-- Use the 'Object' 'Monoid' instance
-- to construct multi-row objects.
{-# INLINE row #-}
row :: Text -> JSON -> Object
row key (JSON value) =
  Object (Just (inline E.row key value))

-- |
-- Array builder from an element,
-- which is an already encoded JSON literal.
-- 
-- Use the 'Array' 'Monoid' instance
-- to construct multi-element arrays.
{-# INLINE element #-}
element :: JSON -> Array
element (JSON value) =
  Array (Just value)

