module JSONBuilder.Model
where

import JSONBuilder.Prelude


data JSON =
  JSON_Null |
  JSON_Number Number |
  JSON_String Text |
  JSON_Boolean Bool |
  JSON_Object Object |
  JSON_Array Array

data Number =
  Number_Int Int |
  Number_Double Double |
  Number_Scientific Scientific

data Object =
  Object_Row Text JSON |
  Object_Append Object Object |
  Object_Empty

instance Monoid Object where
  mempty =
    Object_Empty
  mappend =
    Object_Append

data Array =
  Array_Element JSON |
  Array_Append Array Array |
  Array_Empty

instance Monoid Array where
  mempty =
    Array_Empty
  mappend =
    Array_Append
