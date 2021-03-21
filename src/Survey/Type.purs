module Survey.Type where

import Survey.Internal (EscapeCodeWrapper)

type CursorPosition = Int

type OutputState =
  { cursorPosition :: CursorPosition
  , plainText :: String
  , escapes :: Array EscapeCodeWrapper
  }
