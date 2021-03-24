module Survey.QuestionItem.Text where

import Prelude

import Ansi.Codes (EscapeCode(..), escapeCodeToString)
import Data.Maybe (Maybe(..))
import Data.String (codePointFromChar, length, splitAt)
import Prelude (mempty, ($), (<>), (-))
import Survey.Internal (Key)
import Survey.Operation (evalOperation)
import Survey.Type (CursorPosition, Operation(..), OutputState)

text :: Key -> OutputState -> OutputState
text key = evalOperation $ keyToOperation key

keyToOperation :: Key -> Operation
keyToOperation { name, sequence, ctrl } =
  case ctrl, name of
    _,     Just "backspace" -> DeleteBackward
    _,     Just "delete" -> DeleteUnderCursor
    _,     Just "up" -> DoNothing
    _,     Just "down" -> DoNothing
    false, Just "right" -> MoveRight
    false, Just "left" -> MoveLeft
    true,  Just "right" -> MoveOneWordRight
    true,  Just "left" -> MoveOneWordLeft
    _,     Just "space" -> PrintCharacter " "
    _,     Just "tab" -> DoNothing
    true,  Just "b" -> MoveLeft
    true,  Just "f" -> MoveRight
    true,  Just "d" -> DeleteUnderCursor
    true,  Just "k" -> DeleteUnderCursorToTail
    true,  Just "u" -> DeleteBeforeUnderCursorToHead
    true,  Just "a" -> MoveToHead
    true,  Just "e" -> MoveToTail
    true,  Just "w" -> DeleteOneWordBeforeCursor
    true,  _ -> DoNothing
    false, Nothing -> PrintCharacter sequence -- symbol(!,@,#,$,%,...)
    false, Just name' -> if length name' == 1
                           then PrintCharacter sequence
                           else DoNothing
