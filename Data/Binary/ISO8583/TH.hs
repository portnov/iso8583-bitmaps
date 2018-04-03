{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Data.Binary.Bitmap.TH
-- Copyright   :  (c) Ilya Portnov 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  portnov84@rambler.ru
-- Stability   :  unstable
-- Portability :  not tested
--
-- This module contains QuasiQuoter for declarative description of
-- ISO 8583-based message formats.
--
----------------------------------------------------------------------------

module Data.Binary.ISO8583.TH
  (-- * Usage
   -- $usage
   FieldType (..),
   FieldValue (..),
   Field (..),
   pData,
   string2data,
   binaryQ, binary
  ) where

import Control.Monad
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Text.Parsec.String
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString (ByteString)
import qualified Data.Map as M
import Data.Generics

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lift

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Data.Binary.ISO8583

-- $usage
--
-- Typical usage is:
--
-- > [binary|
-- >   Message
-- >   2 pan embedded 2
-- >   4 amount int 12
-- >   11 stan int 6
-- >   43 termAddress TermAddress 222
-- > |]
--
-- > data TermAddress = TermAddress ...
--
-- > instance Binary TermAddress where ...
--
-- Quasi-quote format is:
--
--  - First line - name of data type to generate
--
--  - Each of other lines describes one field, in following format:
--  {Field number} {Field name} {data type} {type parameter}
--
--  Internally supported data types are: 
--
--  - int - integer field. Parameter defines size of field.
--
--  - str - fixed-length string field. Parameter defines size of field.
--  
--  - embedded - embedded-length string field. Parameter defines number of bytes used to store field length (2 for LLVAR, 3 for LLLVAR and so on).
--
--  Any other data type, instance of Binary class, may be used as well.
--
--  Quasi-quoter generates data type definition, and also following functions:
--
--  - get[Message] :: Int -> Maybe (Get FieldValue)
--
--  - put[Message] :: Message -> [(Int, Maybe Put)]
--
--  - construct[Message] :: M.Map Int FieldValue -> Message
--
-- Concrete ISO 8583-based formats usually use some kind of message header, or 
-- use bitmap only as small part of overall format. So, it's usually no point of
-- generating instance Binary Message - it's better to write instance for your
-- format by using functions mentioned above.
--
-- > instance Binary Message where
-- >   get = do
-- >     -- parse some kind of header here, then:
-- >     m <- getBitmap getMessage
-- >     return $ constructMessage m
-- >   
-- >   put msg = do
-- >     -- write some kind of header here, then:
-- >     putBitmap' (putMessage msg)
--

-- | Supported field types
data FieldType =
       TInt Int          -- ^ Integer field of given size
     | TString Int       -- ^ Fixed-length string field
     | TEmbeddedLen Int  -- ^ Variable-length string field with embedded length (LLVAR and so on)
     | TOther String Int -- ^ User-defined field - any data type with instance Binary. NB: second parameter is not used currently.
  deriving (Eq, Show)

deriveLift ''FieldType

-- | Field format description
data Field =
      Field {
        fNumber :: Int      -- ^ Field number
      , fName :: String     -- ^ Field name
      , fType :: FieldType  -- ^ Field type description
    }
  deriving (Eq, Show)

-- | Supported field values
data FieldValue =
       FInt Integer         -- ^ Integer field
     | FString ByteString   -- ^ String fields
     | forall t. (Binary t, Show t, Typeable t) => FOther t -- ^ User-defined field

instance Eq FieldValue where
  (FInt x) == (FInt y) = x == y
  (FString x) == (FString y) = x == y
  (FOther x) == (FOther y) = runPut (put x) == runPut (put y)
  _ == _ = False

instance Show FieldValue where
  show (FInt x) = show x
  show (FString x) = show x
  show (FOther x) = show $ runPut (put x)

unpackFInt :: FieldValue -> Integer
unpackFInt (FInt x) = x
unpackFInt x = error $ "Internal error: not an integer: " ++ show x

unpackFString :: FieldValue -> ByteString
unpackFString (FString str) = str
unpackFString x = error $ "Internal error: not a string: " ++ show x

toStrict :: L.ByteString -> B.ByteString
toStrict s = B.concat $ L.toChunks s

fromStrict :: B.ByteString -> L.ByteString
fromStrict s = L.fromChunks [s]

unpackOther :: FieldValue -> ByteString
unpackOther (FOther x) = toStrict $ runPut (put x)
unpackOther _ = error $ "Internal error: not a FOther"

lexer = P.makeTokenParser haskellDef
identifier = P.identifier lexer
number = P.natural lexer

pField :: Parser Field
pField = do
  skipMany (space <|> newline)
  n <- number
  skipMany space
  name <- identifier
  skipMany space
  t <- identifier
  skipMany space
  param <- number
  ft <- case t of
          "int" -> return $ TInt (fromIntegral param)
          "str" -> return $ TString (fromIntegral param)
          "embedded" -> return $ TEmbeddedLen (fromIntegral param)
          _ -> return $ TOther t (fromIntegral param)
  return $ Field (fromIntegral n) name ft

pFields :: Parser [Field]
pFields = do
  fs <- pField `sepEndBy1` many newline
  eof
  return fs

-- | Parse data format definition
pData :: Parser (String, [Field])
pData = do
  skipMany (space <|> newline)
  name <- identifier
  skipMany newline
  fs <- pFields
  return (name, fs)

-- | Generate `data Message = Message {...'
generateData :: String    -- ^ Data type name
             -> [Field]   -- ^ Fields description
             -> Q [Dec]
generateData nameStr fields = do
    let name = mkName nameStr
    fields' <- mapM convertField fields
    let constructor = RecC name fields'
    return [ DataD [] name [] Nothing [constructor] [] ]
  where
    convertField (Field _ fname ftype) = do
      t <- convertType ftype
      let bang = Bang NoSourceUnpackedness SourceStrict
      return (mkName fname, bang, t)

    convertType (TInt _) = [t| Maybe Integer |]
    convertType (TString _) = [t| Maybe B.ByteString |]
    convertType (TEmbeddedLen _) = [t| Maybe B.ByteString |]
    convertType (TOther n _) = do
        let name = mkName n
        return $ ConT (mkName "Maybe") `AppT` ConT name

-- | Generate `getMessage 2 = Just $ ...'
mkGetter :: String -> [Field] -> Q [Dec]
mkGetter nameStr fields = do
    let name = mkName nameStr
    clauses <- forM fields $ \(Field f _ ft) -> mkGetClause f ft
    unsupportedClause <- mkUnsupportedClause 
    return [FunD name (clauses ++ [unsupportedClause])]
  where
    mkGetClause :: Int -> FieldType -> Q Clause
    mkGetClause n ft = do
      let pats = [LitP (IntegerL (fromIntegral n))]
      let int x = return $ LitE $ IntegerL (fromIntegral x)
      body <- case ft of 
                TInt sz -> [| Just $ FInt `fmap` asciiNumberF $(int n) $(int sz) |]
                TString sz -> [| Just $ FString `fmap` getByteString $(int sz) |]
                TEmbeddedLen sz -> [| Just $ FString `fmap` embeddedLen $(int n) $(int sz) |]
                TOther name _ -> [| Just $ FOther `fmap` (get :: Get $(return $ ConT $ mkName name) ) |]
      return $ Clause pats (NormalB body) []

    mkUnsupportedClause :: Q Clause
    mkUnsupportedClause = do
      body <- [| Nothing |]
      let pats = [ WildP ]
      return $ Clause pats (NormalB body) []

-- | Generate `putMessage'
mkPutter :: String -> [Field] -> Q [Dec]
mkPutter nameStr fields = do
    msg <- newName "msg"
    let name = mkName nameStr
        pat = VarP msg
    body <- ListE `fmap` mapM (mkBody msg) fields
    let clause = Clause [pat] (NormalB body) []
    return [FunD name [clause]]
  where
    mkBody msg (Field f fname ftype) = do
      let getter = return $ VarE $ mkName fname
      let msgvar = return $ VarE msg
      let wrapper = case ftype of
                     TInt _ -> return $ ConE $ mkName "FInt"
                     TString _    -> return $ ConE $ mkName "FString"
                     TEmbeddedLen _    -> return $ ConE $ mkName "FString"
                     TOther name _ -> return $ ConE $ mkName "FOther"
      let putter = [| putField $(lift ftype) ( $(wrapper) `fmap` $(getter) $(msgvar) ) |]
      [| ( $(lift f) , $(putter) ) |]

putField :: FieldType -> Maybe FieldValue -> Maybe Put
putField _ Nothing = Nothing
putField (TInt sz) (Just (FInt n)) = Just $ putAsciiNumber sz n
putField (TString sz) (Just (FString s)) = Just $ putByteStringPad sz s
putField (TEmbeddedLen sz) (Just (FString s)) = Just $ putEmbeddedLen sz s
putField (TOther _ _) (Just (FOther x)) = Just $ put x
putField t (Just v) = fail $ "Internal error: field value " ++ show v ++ " does not match type " ++ show t

mkConstructor :: String -> String -> [Field] -> Q [Dec]
mkConstructor prefix nameStr fields = do
    msg <- newName "msg"
    let name = mkName nameStr
        pat = VarP msg
    body <- RecConE name `fmap` mapM (mkBody msg) fields
    let clause = Clause [pat] (NormalB body) []
    return [FunD (mkName $ prefix ++ nameStr) [clause]]
  where
    mkBody msg (Field f fname ftype) = do
      let msgvar = return $ VarE msg
      let unpack = case ftype of
                     TInt _ -> [| unpackFInt |]
                     TString _ -> [| unpackFString |]
                     TEmbeddedLen _ -> [| unpackFString |]
                     TOther _ _ -> [| runGet get . fromStrict . unpackOther |]
      rhs <- [| $(unpack) `fmap` M.lookup $(lift f) $(msgvar) |]
      return (mkName fname, rhs)

-- | Generate only data type definition
string2data :: String -> Q [Dec]
string2data str = do
  case parse pData "<input>" str of
    Right (name, fields) -> generateData name fields
    Left err -> fail $ show err

binaryQ :: String -> Q [Dec]
binaryQ str = do
  case parse pData "<input>" str of
    Right (name, fields) -> do
        dtype <- generateData name fields
        getter <- mkGetter ("get" ++ name) fields
        putter <- mkPutter ("put" ++ name) fields
        cons <- mkConstructor "construct" name fields
        return $ dtype ++ getter ++ putter ++ cons
    Left err -> fail $ show err

dataOnly :: QuasiQuoter
dataOnly = QuasiQuoter {quoteDec = string2data, quoteExp=undefined, quotePat=undefined, quoteType=undefined}

-- | Main function here.
binary :: QuasiQuoter
binary = QuasiQuoter {quoteDec = binaryQ,  quoteExp=undefined, quotePat=undefined, quoteType=undefined}

