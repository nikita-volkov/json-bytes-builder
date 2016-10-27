-- |
-- DSL for construction of JSON.
module JSONBytesBuilder.Builder
(
  -- ** Literal builders
  Literal,
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
-- Builder of any JSON literal.
newtype Literal =
  Literal A.Builder

-- |
-- Builder of a JSON Object literal.
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
-- Builder of a JSON Array literal.
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
null :: Literal
null =
  Literal (inline E.null)

-- |
-- JSON Boolean literal from 'Bool'.
{-# INLINE boolean #-}
boolean :: Bool -> Literal
boolean =
  Literal . inline E.boolean

-- |
-- JSON Number literal from 'Int'.
{-# INLINE number_int #-}
number_int :: Int -> Literal
number_int =
  Literal . inline A.intDec

-- |
-- JSON Number literal from 'Integer'.
{-# INLINE number_integer #-}
number_integer :: Integer -> Literal
number_integer =
  Literal . inline A.integerDec

-- |
-- JSON Number literal from 'Double'.
{-# INLINE number_double #-}
number_double :: Double -> Literal
number_double =
  Literal . inline A.doubleDec

-- |
-- JSON Number literal from 'Scientific'.
{-# INLINE number_scientific #-}
number_scientific :: Scientific -> Literal
number_scientific =
  Literal . inline E.scientific

-- |
-- JSON String literal from 'Text'.
{-# INLINE string #-}
string :: Text -> Literal
string =
  Literal . inline E.string

-- |
-- JSON Object literal from the 'Object' builder.
{-# INLINE object #-}
object :: Object -> Literal
object (Object x) =
  Literal (maybe E.emptyObject (inline E.inCurlies) x)

-- |
-- JSON Array literal from the 'Array' builder.
{-# INLINE array #-}
array :: Array -> Literal
array (Array x) =
  Literal (maybe E.emptyArray (inline E.inSquarelies) x)

-- |
-- Object builder from a key-value pair,
-- where value is an already encoded JSON literal.
-- 
-- Use the 'Object' 'Monoid' instance
-- to construct multi-row objects.
{-# INLINE row #-}
row :: Text -> Literal -> Object
row key (Literal literal) =
  Object (Just (inline E.row key literal))

-- |
-- Array builder from an element,
-- which is an already encoded JSON literal.
-- 
-- Use the 'Array' 'Monoid' instance
-- to construct multi-element arrays.
{-# INLINE element #-}
element :: Literal -> Array
element (Literal literal) =
  Array (Just literal)

