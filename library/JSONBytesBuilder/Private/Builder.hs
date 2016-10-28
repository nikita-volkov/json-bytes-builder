module JSONBytesBuilder.Private.Builder
where

import JSONBytesBuilder.Private.Prelude hiding (null)
import qualified Data.ByteString.Builder as A
import qualified JSONBytesBuilder.Private.ByteString.Builder as E


-- |
-- Builder of any JSON literal.
newtype Literal =
  Literal A.Builder

-- |
-- Builder of JSON Object rows.
newtype Rows =
  Rows (Maybe A.Builder)

instance Monoid Rows where
  {-# INLINE mempty #-}
  mempty =
    Rows Nothing
  {-# INLINE mappend #-}
  mappend =
    \case
      Rows (Just left) ->
        \case
          Rows (Just right) ->
            Rows (Just (left <> A.char8 ',' <> right))
          _ ->
            Rows (Just left)
      Rows Nothing ->
        id

-- |
-- Builder of JSON Array elements.
newtype Elements =
  Elements (Maybe A.Builder)

instance Monoid Elements where
  {-# INLINE mempty #-}
  mempty =
    Elements Nothing
  {-# INLINE mappend #-}
  mappend =
    \case
      Elements (Just left) ->
        \case
          Elements (Just right) ->
            Elements (Just (left <> A.char8 ',' <> right))
          _ ->
            Elements (Just left)
      Elements Nothing ->
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
-- JSON Object literal from the 'Rows' builder.
{-# INLINE object #-}
object :: Rows -> Literal
object (Rows x) =
  Literal (maybe E.emptyObject (inline E.inCurlies) x)

-- |
-- JSON Array literal from the 'Elements' builder.
{-# INLINE array #-}
array :: Elements -> Literal
array (Elements x) =
  Literal (maybe E.emptyArray (inline E.inSquarelies) x)

-- |
-- Rows builder from a key-value pair,
-- where value is an already encoded JSON literal.
-- 
-- Use the 'Rows' 'Monoid' instance
-- to construct multi-row objects.
{-# INLINE row #-}
row :: Text -> Literal -> Rows
row key (Literal literal) =
  Rows (Just (inline E.row key literal))

-- |
-- Elements builder from an element,
-- which is an already encoded JSON literal.
-- 
-- Use the 'Elements' 'Monoid' instance
-- to construct multi-element arrays.
{-# INLINE element #-}
element :: Literal -> Elements
element (Literal literal) =
  Elements (Just literal)

