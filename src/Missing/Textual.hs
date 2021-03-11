
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Description     : Textual representation
Stability       : experimental

Various types of text can be handled similarly.
-}
module Missing.Textual
    (   -- * Textual
        Textual(..)
    ,   foldText
    ,   foldText'

    ,   -- * Wrappers for specific encodings
        ASCIIBuilder(..)
    ,   UTF8Builder
    ) where

import Prelude hiding (concat)
import qualified Prelude as P
import Data.Foldable as F

import Control.Monad.Writer
import Data.Text as T
import Data.Text.Encoding
import Data.ByteString as B
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder

{-----------------------------------------------------------------------------}

foldText :: (Textual t, Foldable f) => (a -> t) -> t -> f a -> t
foldText totext space = F.foldl' (\rest e -> rest <> space <> totext e) mempty

foldText' :: (Textual t, Foldable f) => f t -> t
foldText' = foldText id (tChar ' ')

{- | A monad for textual representations

e.g. ASCII or UTF-8
-}
class Monoid m => Textual m where
    {- | Convert back to a string
    
    /Attention/: This makes no guarantees, because
    data might be lost during encoding.
    -}
    textualToString :: m -> String

    -- | Encode single character
    tChar :: Char -> m

    {-# MINIMAL textualToString, tChar #-}

    textualToText :: m -> Text
    textualToText = T.pack . textualToString

    -- | Encode a string
    tString :: String -> m
    tString = foldMap tChar

    -- | Encode a newline
    tNewline :: m
    tNewline = tChar '\n'

    -- | Encode an 'Int' in decimal represensation
    tIntDec :: Int -> m
    tIntDec = tString . show

    -- | Encode an 'Integer' in decimal represensation
    tIntegerDec :: Integer -> m
    tIntegerDec = tString . show

instance Textual String where
    textualToString = id
    tChar = pure
    tString = id

instance Textual [String] where
    textualToString = P.concat
    tChar = pure . pure
    tString = pure

instance Textual ShowS where
    textualToString = ($ "")
    tChar = showChar
    tString = showString

{-----------------------------------------------------------------------------}

-- | A type for building ASCII text
newtype ASCIIBuilder = ASCIIBuilder { unASCIIBuilder :: Builder }

--deriving instance Show ASCIIBuilder
deriving instance Semigroup ASCIIBuilder
deriving instance Monoid ASCIIBuilder

instance Textual Text where
    textualToString = T.unpack
    textualToText = id
    tChar = T.singleton
    tString = T.pack

-- There has to be a way to get a STRICT bytestring...
toStrictByteString :: Builder -> ByteString
toStrictByteString = toStrict . toLazyByteString

-- ByteString builder
instance Textual ASCIIBuilder where
    textualToString = T.unpack . textualToText
    tChar = ASCIIBuilder . char7
    tString = ASCIIBuilder . string7
    textualToText = decodeUtf8 . toStrictByteString . unASCIIBuilder
    tIntDec = ASCIIBuilder . intDec
    tIntegerDec = ASCIIBuilder . integerDec

{-----------------------------------------------------------------------------}

-- | A type for building UTF-8 text
newtype UTF8Builder = UTF8Builder { unUTF8Builder :: Builder }

--deriving instance Show UTF8Builder
deriving instance Semigroup UTF8Builder
deriving instance Monoid UTF8Builder

instance Textual UTF8Builder where
    textualToString = T.unpack . textualToText
    tChar = UTF8Builder . char8
    tString = UTF8Builder . string8
    textualToText = decodeUtf8 . toStrictByteString . unUTF8Builder
    tIntDec = UTF8Builder . intDec
    tIntegerDec = UTF8Builder . integerDec

{-----------------------------------------------------------------------------}
