{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Control.Lens.Missing
  ( -- * URI
    _uriScheme
  , _uriAuthority
  , _uriPath
  , _uriQuery
  , _uriFragment
    -- ** Authority
  , _uriUserInfo
  , _uriRegName
  , _uriPort
    -- * NonEmpty
  , _NonEmptyHead
  , _NonEmptyLast
  , _NonEmptyList
    -- * Text
  , utf8
  , notNullText
    -- * Email Address
  , emailAddressBytes
  , emailAddressText
    -- * UUID
  , uuidText
    -- * API ints
  , apiInt
  , apiInteger
  -- * Coercible
  , coercing
  ) where

import Control.Lens
import Data.ByteString as BS
import Data.Coerce
import Data.List as L
import Data.List.NonEmpty as NE
import Data.Scientific
import Data.Text as T
import Data.Text.Encoding as T
import Data.UUID
import Network.URI
import Text.Email.Validate as Email


makeLensesFor
  (L.map (\a -> (a, "_" ++ a))
    ["uriScheme", "uriAuthority", "uriPath", "uriQuery", "uriFragment"])
    ''URI

makeLensesFor
  (L.map (\a -> (a, "_" ++ a))
    ["uriUserInfo", "uriRegName", "uriPort"])
    ''URIAuth

_NonEmptyHead :: Lens' (NonEmpty a) a
_NonEmptyHead f (a :| as) = f a <&> \a' -> a' :| as

_NonEmptyLast :: Lens' (NonEmpty a) a
_NonEmptyLast = reversed . _NonEmptyHead

_NonEmptyList :: Prism' [a] (NonEmpty a)
_NonEmptyList = prism' NE.toList nonEmpty

-- | Utf8-encoding @ByteString@-@Text@ @Iso@morphism.
utf8 :: Iso' Text BS.ByteString
utf8 = iso encodeUtf8 decodeUtf8

-- | Prism returns text if it is not empty.
notNullText :: Prism' Text Text
notNullText = prism id $ \t -> if T.null t then Left t else Right t

uuidText :: Prism' Text UUID
uuidText = prism' (T.pack . toString) (fromString . T.unpack)

emailAddressBytes :: Prism' ByteString EmailAddress
emailAddressBytes = prism' Email.toByteString emailAddress

emailAddressText :: Prism' Text EmailAddress
emailAddressText = utf8 . emailAddressBytes

apiInt :: Getter Int Scientific
apiInt = to (flip scientific 0 . toInteger)

apiInteger :: Getter Integer Scientific
apiInteger = to (flip scientific 0)

-- | A @Simple@ version of @coerced@.
--
-- This can be useful when you need to explicitly specify the types.
--
-- >>> foo ^. pnr . coercing @APIPnr @Pnr -- coercing from APIPnr to Pnr
--
-- This would be cumbersome to explicitly specify using @coerced@:
-- >>> foo ^. pnr . coerced @APIPnr @APIPnr @Pnr
coercing :: forall s a. (Coercible s a) => Iso' s a
coercing = coerced
