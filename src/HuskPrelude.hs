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

    -- ** Arrow
  , Control.Arrow.first
  , Control.Arrow.second
  , (Control.Arrow.***)
  , (Control.Arrow.&&&)
  )
  where

import Prelude
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy
import Data.Text (Text)
import qualified Data.Text.Lazy
import Data.String.Conversions (cs)
import qualified Control.Arrow


type LText =
  Data.Text.Lazy.Text

type LByteString =
  Data.ByteString.Lazy.ByteString

infixr 0 <|
infixl 0 |>

(<|) = ($)
(|>) = flip ($)
