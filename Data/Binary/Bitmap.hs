----------------------------------------------------------------------------
-- |
-- Module      :  Data.Binary.Bitmap
-- Copyright   :  (c) Ilya Portnov 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  portnov84@rambler.ru
-- Stability   :  unstable
-- Portability :  not tested
--
-- This module contains Get and Put method implementations for ISO 8583 style
-- bitmaps, and also utility methods for getting/putting fields in formats
-- commonly used in ISO 8583 specification.
--
----------------------------------------------------------------------------

module Data.Binary.Bitmap
 (-- * Usage
  -- $usage
  getBitmapFieldNumbers, 
  getBitmap,
  mergeFieldNumbers,
  putBitmap, putBitmap',
  embeddedLen,
  asciiNumber, asciiNumberF,
  putAsciiNumber, putEmbeddedLen,
  putByteStringPad, putLazyByteStringPad,
  toBS, fromBS ) where

import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8
import Data.Int
import Data.Word
import Data.Bits
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Text.Printf

--import Debug.Trace

-- $usage
--
-- Typical usage is:
--
-- > data Message = Message {pan :: B.ByteString, stan :: Integer}
-- > deriving (Eq, Show)
--
-- > data FieldValue = String B.ByteString | Int Integer
-- > deriving (Eq, Show)
--
-- > getField :: Int -> Maybe (Get FieldValue)
-- > getField 2 = Just $ String `fmap` embeddedLen 2
-- > getField 11 = Just $ Int `fmap` asciiNumberF 11 6
-- > getField _ = Nothing
--
-- > getMessage :: Get Message
-- > getMessage = do
-- >   m <- getBitmap getField
-- >   let Just (String pan) = M.lookup 2 m
-- >   let Just (Int stan) = M.lookup 11 m
-- >   return $ Message pan stan
--
-- > putMessage :: Message -> Put
-- > putMessage (Message pan stan) = do
-- >   putBitmap [(2, putEmbeddedLen 2 pan),
-- >              (11, putAsciiNumber 6 stan)] 
--

bytes :: [(Word8, Word8)]
bytes = let digits = "0123456789ABCDEF"
        in  [(fromIntegral (ord digit), n) | (digit, n) <- zip digits [0..]]

bits :: [(Word8, [Int])]
bits = [(digit, [i | i <- [1..4] , testBit n (4-i)]) | (digit, n) <- bytes]

bitsSet :: Int -> Word8 -> [Int]
bitsSet byteNr b = 
  let fs = case lookup b bits of
             Just x -> x
             Nothing -> error $ "Unknown byte: " ++ [chr (fromIntegral b)]
  in [4*byteNr + n | n <- fs]

-- | Parse bitmap. Return numbers of fields present.
-- NB: only two bitmaps are supported as for now (Primary and Secondary bitmaps in ISO 8583 notation).
getBitmapFieldNumbers :: Get [Int]
getBitmapFieldNumbers = do
  bm <- getByteString 16
  let xs = [bitsSet i b | (i,b) <- zip [0..] (B.unpack bm)]
  return $ concat xs

mergeFieldNumbersW :: [Int] -> [Word64]
mergeFieldNumbersW fs =
  let (fs1,fs2) = partition (<65) fs
      go b f = setBit b (63-f)
      w1 = foldl go 0 $ map (\f -> f - 1) fs1
      w2 = foldl go 0 $ map (\f -> f - 65) fs2
  in  if w2 == 0
        then [w1]
        else [setBit w1 63, w2]

-- | Merge numbers of fields present into ISO bitmap.
mergeFieldNumbers :: [Int] -> B.ByteString
mergeFieldNumbers fs =
  -- TODO: usage of toUpper . printf for hexadecimal number is pretty slow.
  let hex n = toBS $ map toUpper $ printf "%016x" n
  in case mergeFieldNumbersW fs of
       [w1] -> hex w1
       [w1,w2] -> hex w1 `B.append` hex w2

-- | Parse ISO 8583-style bitmap.
-- Fails if unsupported field is present in message.
getBitmap :: (Int -> Maybe (Get f))  -- ^ Parser for n'th field, or Nothing if field is not supported
          -> Get (M.Map Int f)
getBitmap getter = do
  fs1 <- getBitmapFieldNumbers
  fs2 <- if 1 `elem` fs1
          then getBitmapFieldNumbers
          else return []
  let fs = fs1 ++ map (64+) fs2
  res <- forM fs $ \f ->
            if f == 1
              then return []
              else
                case getter f of
                  Nothing -> fail $ "Unsupported field #" ++ show f
                  Just fn -> do
                    offset <- bytesRead
                    --trace ("Parsing field #" ++ show f ++ " at " ++ show offset) $ return ()
                    fn >>= (\x -> return [(f, x)])
  return $ M.fromList $ concat res
               
-- | Put ISO 8583-style bitmap.
putBitmap :: [(Int, Put)] -- ^ (Field number, field putter)
          -> Put
putBitmap fs = do
  let fieldNumbers = map fst fs
  --trace ("Putting fields: " ++ show fieldNumbers) $ return ()
  putByteString $ mergeFieldNumbers fieldNumbers
  mapM_ snd fs

-- | Put ISO 8583-style bitmap.
putBitmap' :: [(Int, Maybe Put)] -- ^ (Field number, field putter or Nothing if field is not present)
           -> Put
putBitmap' fs = do
  let fs' = [(f, p) | (f, Just p) <- fs]
  let fieldNumbers = map fst fs'
  --trace ("Putting fields: " ++ show fieldNumbers) $ return ()
  putByteString $ mergeFieldNumbers fieldNumbers
  mapM_ snd fs'

-- | Parse string with embedded length (LLVAR/LLLVAR in ISO 8583 notation)
embeddedLen :: Int              -- ^ Field number (to be used in error message)
            -> Int              -- ^ Number of bytes used for length (2 for LLVAR, 3 for LLLVAR and so on)
            -> Get B.ByteString
embeddedLen f n = do
  sz <- asciiNumberF f n
  getByteString (fromIntegral sz)

-- | Parse number of given length in ASCII notation; 
-- Report bitmap field number in case of error.
asciiNumberF :: Int          -- ^ Field number, to be used in error message
             -> Int          -- ^ Number length
             -> Get Integer
asciiNumberF f n = do
  bs <- getByteString n
  case C8.readInteger bs of
    Just (res, s)
      | C8.null s -> return res
    _ -> fail $ "Cannot parse number: <" ++ fromBS bs ++ "> in field #" ++ show f

-- | Parse number of given length in ASCII notation
asciiNumber :: Int      -- ^ Number length
            -> Get Int
asciiNumber n = do
  bs <- getByteString n
  case reads (fromBS bs) of
    [(res, "")] -> return res
    _ -> fail $ "Cannot parse number: " ++ fromBS bs

-- | Put number of given length in ASCII notation
putAsciiNumber :: Int      -- ^ Number length
               -> Integer  -- ^ Number
               -> Put
putAsciiNumber sz n = do
  let s = show n
      m = length s
      s' = if m < sz
             then replicate (sz-m) '0' ++ s
             else if m == sz
                    then s
                    else drop (m-sz) s
      bs = toBS s'
  putByteString bs

-- | Put string with embedded length (LLVAR/LLLVAR in ISO 8583 notation)
putEmbeddedLen :: Int          -- ^ Number of bytes used for length (2 for LLVAR, 3 for LLLVAR and so on)
               -> B.ByteString -- ^ String to put
               -> Put
putEmbeddedLen sz bstr = do
  let len = fromIntegral $ B.length bstr
  putAsciiNumber sz len
  putByteString bstr

-- | Put space-padded string of given length
putByteStringPad :: Int           -- ^ Field length
                 -> B.ByteString  -- ^ String to put
                 -> Put
putByteStringPad sz bstr = do
  let len = B.length bstr
  let bstr' = if len < sz
                then B.replicate (sz-len) 0x20 `B.append` bstr
                else bstr
  putByteString bstr'

-- | Put space-padded string of given length
putLazyByteStringPad :: Int64           -- ^ Field length
                 -> L.ByteString  -- ^ String to put
                 -> Put
putLazyByteStringPad sz bstr = do
  let len = L.length bstr
  let bstr' = if len < sz
                then L.replicate (sz-len) 0x20 `L.append` bstr
                else bstr
  putLazyByteString bstr'

toBS :: String -> B.ByteString
toBS str = B.pack $ map (fromIntegral . ord) str

fromBS :: B.ByteString -> String
fromBS bstr = map (chr . fromIntegral) $ B.unpack bstr

