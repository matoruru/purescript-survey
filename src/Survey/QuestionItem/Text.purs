module Survey.QuestionItem.Text ( text ) where

import Ansi.Codes (EscapeCode(..), escapeCodeToString)
import Data.Maybe (Maybe(..))
import Data.String (splitAt)
import Prelude (mempty, ($), (<>), (-))
import Survey.Internal (Key)
import Survey.Type (CursorPosition, OutputFormat)

text :: Key -> OutputFormat -> OutputFormat
text
  { name, sequence, ctrl }
  outputFormat@
    { cursorPosition
    , plainText
    , escapes
    , operations
    } = case name of
  Just "backspace" -> do
    outputFormat
    --let a s = do
    --            let { before, after } = splitAt cursorPosition plainText
    --            
    --outputFormat
    --  { cursorPosition = cursorPosition - 1
    --  , operations = operations <> [  ]
    --  }
  Just "up" -> outputFormat
  Just "down" -> outputFormat
  Just "left" -> outputFormat --escapeCodeToString $ Back 1
  Just "right" -> outputFormat --escapeCodeToString $ Forward 1
  _ -> outputFormat --sequence

--deleteOneBeforeCursor :: CursorPosition -> String -> String
--deleteOneBeforeCursor cursorPosition s = do
--  let { before, after } = splitAt (cursorPosition - 1) plainText

--01234
--abcd

