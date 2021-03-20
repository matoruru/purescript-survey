module Survey.Internal where

import Data.Maybe (Maybe)

-- | This object is coming from 'keypress' event. 
type Key =
  { sequence :: String
  , name     :: Maybe String
  , ctrl     :: Boolean
  , meta     :: Boolean
  , shift    :: Boolean
  }
