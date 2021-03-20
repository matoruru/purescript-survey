module Survey.QuestionItem.Text ( text ) where

import Ansi.Codes (EscapeCode(..), escapeCodeToString)
import Data.Maybe (Maybe(..))
import Prelude (mempty, ($), (<>))
import Survey.Internal (Key)

text :: Key -> String
text { name, ctrl } = case name of
  Just "backspace" -> deleteOneBeforeCursor
  Just "up" -> mempty
  Just "down" -> mempty
  Just "s" -> escapeCodeToString SavePosition
  Just "r" -> escapeCodeToString RestorePosition
  s -> escapeCodeToString QueryPosition

deleteOneBeforeCursor :: String
deleteOneBeforeCursor = back1 <> " " <> back1
  where
    back1 = (escapeCodeToString $ Back 1)
