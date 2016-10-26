module JSONBuilder.Builder
where

import JSONBuilder.Prelude
import JSONBuilder.Model


boolean :: Bool -> JSON
boolean =
  JSON_Boolean

object :: Object -> JSON
object =
  JSON_Object

row :: Text -> JSON -> Object
row =
  Object_Row

element :: JSON -> Array
element =
  Array_Element
