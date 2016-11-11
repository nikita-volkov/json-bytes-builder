{-# LANGUAGE NoImplicitPrelude #-}

import BasePrelude
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified JSONBytesBuilder.Builder as A
import qualified JSONBytesBuilder.ByteString.ByteString as B
import qualified Data.ByteString.Char8 as C


-- |
-- Outputs the following:
-- 
-- >{"name":"Metallica","genres":[{"name":"Metal"},{"name":"Rock"},{"name":"Blues"}]}
main =
  C.putStrLn (B.jsonLiteral metallica)


-- * Model
-------------------------

data Artist =
  Artist { artist_name :: Text, artist_genres :: [Genre] }

data Genre =
  Genre { genre_name :: Text }


-- * Builders
-------------------------

artist :: Artist -> A.Literal
artist (Artist name genres) =
  A.object rows
  where
    rows =
      A.row "name" (A.string_text name) <>
      A.row "genres" (A.array genresElements)
      where
        genresElements =
          foldMap (A.element . genre) genres

genre :: Genre -> A.Literal
genre (Genre name) =
  A.object rows
  where
    rows =
      A.row "name" (A.string_text name)

metallica :: A.Literal
metallica =
  artist (Artist "Metallica" [Genre "Metal", Genre "Rock", Genre "Blues"])
