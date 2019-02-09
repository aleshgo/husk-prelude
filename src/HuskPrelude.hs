module HuskPrelude
  ( -- * Basic
    module Prelude

    -- * Functions
  , (<|)
  , (|>)

    -- * Text
  , ByteString
  , LByteString
  , Text
  , LText
  , cs
  )
  where

import Prelude
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy
import Data.Text (Text)
import qualified Data.Text.Lazy
import Data.String.Conversions (cs)


type LText =
  Data.Text.Lazy.Text

type LByteString =
  Data.ByteString.Lazy.ByteString

infixr 0 <|
infixl 0 |>

(<|) = ($)
(|>) = flip ($)
