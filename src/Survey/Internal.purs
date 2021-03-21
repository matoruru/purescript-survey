module Survey.Internal where

import Prelude

import Ansi.Codes (EscapeCode, escapeCodeToString)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

-- | This object is coming from 'keypress' event.
-- | To know more avout a value of sequence
-- | see: https://en.wikipedia.org/wiki/Escape_sequences_in_C
type Key =
  { sequence :: String
  , name     :: Maybe String
  , ctrl     :: Boolean
  , meta     :: Boolean
  , shift    :: Boolean
  }

-- | To deal with the lacking of Show instance for EscapeCode.
newtype EscapeCodeWrapper = EscapeCodeWrapper EscapeCode

instance showEscapeCodeWrapper :: Show EscapeCodeWrapper where
  show (EscapeCodeWrapper escapeCode)= "EscapeCodeWrapper " <> escapeCodeToString escapeCode

instance eqEscapeCodeWrapper :: Eq EscapeCodeWrapper where
  eq (EscapeCodeWrapper escapeCodea) (EscapeCodeWrapper escapeCodeb) = escapeCodea == escapeCodeb

derive instance newtypeEscapeCodeWrapper :: Newtype EscapeCodeWrapper _
