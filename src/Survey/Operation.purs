module Survey.Operation where

import Prelude

import Ansi.Codes (EscapeCode(..), escapeCodeToString)
import Data.Eq.Generic (genericEq)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Newtype (wrap)
import Data.Show.Generic (genericShow)
import Data.String (length, splitAt)
import Survey.Internal (EscapeCodeWrapper(..))
import Survey.Type (CursorPosition, OutputState)
import Survey.Util (aggregateMovements)

data Operation
  = DeleteUnderCursor
  | DeleteBackward
  | DeleteUnderCursorToTail
  | DeleteBeforeUnderCursorToHead
  | DeleteAWordBeforeCursor
  | PrintCharacter String
  | MoveRight
  | MoveLeft
  | MoveToTail
  | MoveToHead
  | DoNothing

derive instance genericOperation :: Generic Operation _

instance showOperation :: Show Operation where
  show = genericShow

instance eqOperation :: Eq Operation where
  eq = genericEq

evalOperation :: Operation -> OutputState -> OutputState
evalOperation = case _ of
  DeleteUnderCursor -> \os ->
    if (os.cursorPosition - 1) < length os.plainText
      then
        os { plainText = (splitAt (os.cursorPosition - 1) os.plainText).before <>
                         (splitAt os.cursorPosition os.plainText).after
           , escapes = aggregateMovements $ os.escapes <> [ wrap $ Forward 1 ]
           }
      else identity os
  DeleteBackward -> \os ->
    os { plainText = (splitAt (os.cursorPosition - 2) os.plainText).before <>
                     (splitAt (os.cursorPosition - 1) os.plainText).after
       , cursorPosition = if os.cursorPosition < 2 then 1 else os.cursorPosition - 1
       }
  DeleteUnderCursorToTail -> \os ->
    if (os.cursorPosition - 1) < length os.plainText
      then do
        let { before, after }= splitAt (os.cursorPosition - 1) os.plainText
        os { plainText = before
           , escapes = aggregateMovements $ os.escapes <> [ wrap $ Forward $ length after ]
           }
      else identity os
  DeleteBeforeUnderCursorToHead -> identity
  DeleteAWordBeforeCursor -> identity
  PrintCharacter c -> \os -> do
    let { before, after } = splitAt (os.cursorPosition - 1) os.plainText
    os { plainText = before <> c <> after
       , cursorPosition = os.cursorPosition + 1
       }
  MoveRight -> \os ->
    if (os.cursorPosition - 1) < length os.plainText
      then
        os { cursorPosition = os.cursorPosition + 1
           , escapes = aggregateMovements $ os.escapes <> [ wrap $ Forward 1 ]
           }
      else identity os
  MoveLeft -> \os ->
    if os.cursorPosition > 1
      then
        os { cursorPosition = os.cursorPosition - 1
           , escapes = aggregateMovements $ os.escapes <> [ wrap $ Back 1 ]
           }
      else identity os
  MoveToTail -> identity
  MoveToHead -> identity
  DoNothing -> identity
