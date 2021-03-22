module Survey.QuestionItem.Text where

import Prelude

import Ansi.Codes (EscapeCode(..), escapeCodeToString)
import Data.Maybe (Maybe(..))
import Data.String (codePointFromChar, length, splitAt)
import Prelude (mempty, ($), (<>), (-))
import Survey.Internal (Key)
import Survey.Operation (Operation(..), evalOperation)
import Survey.Type (CursorPosition, OutputState)

text :: Key -> OutputState -> OutputState
text key = evalOperation $ keyToOperation key

keyToOperation :: Key -> Operation
keyToOperation { name, sequence, ctrl } =
  case name of
    Just "backspace" -> DeleteBackward
    Just "up" -> DoNothing
    Just "down" -> DoNothing
    Just "right" -> MoveRight
    Just "left" -> MoveLeft
    _ ->
      case ctrl of
        true -> DoNothing
        false -> PrintCharacter sequence
        --Nothing -> evalOperation $ PrintCharacter sequence

        --Just name' ->
        --  case name' of
        --    "up" -> evalOperation DoNothing
        --    "down" -> evalOperation DoNothing
        --    "left" -> evalOperation DoNothing
        --    "right" -> evalOperation DoNothing
        --_ -> evalOperation DoNothing
