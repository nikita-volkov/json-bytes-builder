module JSONBytesBuilder.Private.Builder
where

import JSONBytesBuilder.Private.Prelude hiding (null)
import qualified Data.ByteString.Builder as A
import qualified Data.Text.Lazy as C
import qualified Data.ByteString.Lazy as D
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

instance Semigroup Rows

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

instance Semigroup Elements

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
{-# INLINE numberFromInt #-}
numberFromInt :: Int -> Literal
numberFromInt =
  Literal . inline A.intDec

-- |
-- JSON Number literal from 'Integer'.
{-# INLINE numberFromInteger #-}
numberFromInteger :: Integer -> Literal
numberFromInteger =
  Literal . inline A.integerDec

-- |
-- JSON Number literal from 'Double'.
{-# INLINE numberFromDouble #-}
numberFromDouble :: Double -> Literal
numberFromDouble =
  Literal . inline A.doubleDec

-- |
-- JSON Number literal from 'Scientific'.
{-# INLINE numberFromScientific #-}
numberFromScientific :: Scientific -> Literal
numberFromScientific =
  Literal . inline E.scientific

-- |
-- JSON String literal from 'Text' encoded using UTF8.
{-# INLINE stringFromText #-}
stringFromText :: Text -> Literal
stringFromText =
  Literal . inline E.stringFromText

-- |
-- JSON String literal from lazy Text encoded using UTF8.
{-# INLINE stringFromLazyText #-}
stringFromLazyText :: C.Text -> Literal
stringFromLazyText =
  Literal . inline E.stringFromLazyText

-- |
-- JSON String literal from 'ByteString' with only escaping applied to it.
{-# INLINE stringFromBytes #-}
stringFromBytes :: ByteString -> Literal
stringFromBytes =
  Literal . inline E.stringFromBytes

-- |
-- JSON String literal from lazy ByteString with only escaping applied to it.
{-# INLINE stringFromLazyBytes #-}
stringFromLazyBytes :: D.ByteString -> Literal
stringFromLazyBytes =
  Literal . inline E.stringFromLazyBytes

-- |
-- JSON Object literal from the 'Rows' builder.
{-# INLINE object #-}
object :: Rows -> Literal
object (Rows x) =
  Literal (maybe E.emptyObject (inline E.inCurlies) x)

-- |
-- JSON Object literal from a list of rows.
-- A convienience shortcut to @object . mconcat@ for typical cases.
{-# INLINE objectFromRows #-}
objectFromRows :: [Rows] -> Literal
objectFromRows =
  object . mconcat

-- |
-- JSON Array literal from the 'Elements' builder.
{-# INLINE array #-}
array :: Elements -> Literal
array (Elements x) =
  Literal (maybe E.emptyArray (inline E.inSquarelies) x)

-- |
-- JSON Array literal from a list of element literals.
-- A convienience shortcut to @array . foldMap element@ for typical cases.
arrayFromLiterals :: [Literal] -> Literal
arrayFromLiterals =
  array . foldMap element

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

