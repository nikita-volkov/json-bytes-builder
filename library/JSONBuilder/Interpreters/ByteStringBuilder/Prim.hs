module JSONBuilder.Interpreters.ByteStringBuilder.Prim
where

import JSONBuilder.Prelude
import Data.ByteString.Builder.Prim


{-# INLINE null #-}
null :: BoundedPrim ()
null =
  ascii4 ('n', ('u', ('l', 'l')))

{-# INLINE boolean #-}
boolean :: BoundedPrim Bool
boolean =
  condB id (ascii4 ('t', ('r', ('u', 'e')))) (ascii5 ('f', ('a', ('l', ('s', 'e')))))

{-# INLINE stringEncodedByte #-}
stringEncodedByte :: BoundedPrim Word8
stringEncodedByte =
  condB (== (fromIntegral . ord) '\\') (ascii2 ('\\', '\\')) $
  condB (== (fromIntegral . ord) '\"') (ascii2 ('\\', '"')) $
  condB (>= (fromIntegral . ord) '\x20') (liftFixedToBounded word8) $
  condB (== (fromIntegral . ord) '\n') (ascii2 ('\\', 'n')) $
  condB (== (fromIntegral . ord) '\r') (ascii2 ('\\', 'r')) $
  condB (== (fromIntegral . ord) '\t') (ascii2 ('\\', 't')) $
  hexEncodedByte

{-# INLINE hexEncodedByte #-}
hexEncodedByte :: BoundedPrim Word8
hexEncodedByte =
  liftFixedToBounded ((\c -> ('\\', ('u', fromIntegral c))) >$< char8 >*< char8 >*< word16HexFixed)

{-# INLINE ascii2 #-}
ascii2 :: (Char, Char) -> BoundedPrim a
ascii2 cs =
  liftFixedToBounded $
  const cs >$< char7 >*< char7

{-# INLINE ascii4 #-}
ascii4 :: (Char, (Char, (Char, Char))) -> BoundedPrim a
ascii4 cs =
  liftFixedToBounded $
  const cs >$< char7 >*< char7 >*< char7 >*< char7

{-# INLINE ascii5 #-}
ascii5 :: (Char, (Char, (Char, (Char, Char)))) -> BoundedPrim a
ascii5 cs =
  liftFixedToBounded $
  const cs >$< char7 >*< char7 >*< char7 >*< char7 >*< char7

{-# INLINE ascii6 #-}
ascii6 :: (Char, (Char, (Char, (Char, (Char, Char))))) -> BoundedPrim a
ascii6 cs =
  liftFixedToBounded $ 
  const cs >$< char7 >*< char7 >*< char7 >*< char7 >*< char7 >*< char7

{-# INLINE ascii8 #-}
ascii8 :: (Char, (Char, (Char, (Char, (Char, (Char, (Char, Char))))))) -> BoundedPrim a
ascii8 cs =
  liftFixedToBounded $
  const cs >$< char7 >*< char7 >*< char7 >*< char7 >*< char7 >*< char7 >*< char7 >*< char7
