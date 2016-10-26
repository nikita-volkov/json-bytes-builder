module JSONBuilder.Builder
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

import JSONBuilder.Prelude hiding (null)
import JSONBuilder.Model


{-# INLINE null #-}
null :: JSON
null =
  JSON_Null

{-# INLINE boolean #-}
boolean :: Bool -> JSON
boolean =
  JSON_Boolean

{-# INLINE number_int #-}
number_int :: Int -> JSON
number_int =
  JSON_Number . Number_Int

{-# INLINE number_double #-}
number_double :: Double -> JSON
number_double =
  JSON_Number . Number_Double

{-# INLINE number_scientific #-}
number_scientific :: Scientific -> JSON
number_scientific =
  JSON_Number . Number_Scientific

{-# INLINE string #-}
string :: Text -> JSON
string =
  JSON_String

{-# INLINE object #-}
object :: Object -> JSON
object =
  JSON_Object

{-# INLINE array #-}
array :: Array -> JSON
array =
  JSON_Array

{-# INLINE row #-}
row :: Text -> JSON -> Object
row =
  Object_Row

{-# INLINE element #-}
element :: JSON -> Array
element =
  Array_Element

