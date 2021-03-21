module Survey.Type where

import Ansi.Codes (EscapeCode)

type CursorPosition = Int

type OutputFormat =
  { cursorPosition :: CursorPosition
  , plainText :: String
  , escapes :: Array EscapeCode
  , operations :: Array (String -> String)
  }
