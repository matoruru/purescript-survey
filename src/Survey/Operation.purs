module Survey.Operation where

import Prelude

import Ansi.Codes (EscapeCode(..), escapeCodeToString)
import Control.Monad.State (get, gets, modify, modify_, put, runState)
import Data.Array (findIndex, head, length, null, reverse, splitAt)
import Data.Eq.Generic (genericEq)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Data.Show.Generic (genericShow)
import Data.String (length, splitAt) as String
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Utils (trimEnd, trimStart)
import Data.Tuple (fst)
import Survey.Internal (EscapeCodeWrapper(..))
import Survey.Type (CursorPosition, Operation(..), OutputState)
import Survey.Util (aggregateMovements, findFirstTailFrom)

evalOperation :: Operation -> OutputState -> OutputState
evalOperation = case _ of
  DeleteUnderCursor -> \os ->
    if os.cursorPosition <= String.length os.plainText
      then
        os { plainText = (String.splitAt (os.cursorPosition - 1) os.plainText).before <>
                         (String.splitAt os.cursorPosition os.plainText).after
           , escapes = aggregateMovements $ os.escapes <> [ wrap $ Forward 1 ]
           }
      else identity os
  DeleteBackward -> \os ->
    os { plainText = (String.splitAt (os.cursorPosition - 2) os.plainText).before <>
                     (String.splitAt (os.cursorPosition - 1) os.plainText).after
       , cursorPosition = if os.cursorPosition < 2 then 1 else os.cursorPosition - 1
       }
  DeleteUnderCursorToTail -> \os ->
    if os.cursorPosition <= String.length os.plainText
      then do
        let { before, after } = String.splitAt (os.cursorPosition - 1) os.plainText
        os { plainText = before
           , escapes = aggregateMovements $ os.escapes <> [ wrap $ Forward $ String.length after ]
           }
      else identity os
  DeleteBeforeUnderCursorToHead -> \os ->
    if os.cursorPosition > 1
      then do
        let { before, after } = String.splitAt (os.cursorPosition - 1) os.plainText
        os { plainText = after
           , escapes = aggregateMovements $ [ wrap $ Back $ String.length after ]
           , cursorPosition = 1
           }
      else identity os
  DeleteOneWordBeforeCursor -> \os ->
    if os.cursorPosition > 1
      then do
        let { before, after } = String.splitAt (os.cursorPosition - 1) os.plainText
            before' = reverse $ toCharArray $ trimEnd $ before
            mIndexOfWordhead = findIndex (\c -> c == ' ') $ before'
            indexOfWordhead = case mIndexOfWordhead of
                                Nothing -> length before'
                                Just idx -> idx
            remains = fromCharArray $ reverse $ (splitAt indexOfWordhead before').after
        os { plainText = remains <> after
           , escapes = aggregateMovements $ [ wrap $ Back $ String.length $ after ]
           , cursorPosition = (String.length remains) + 1
           }
      else identity os
  PrintCharacter c -> \os -> do
    let { before, after } = String.splitAt (os.cursorPosition - 1) os.plainText
    os { plainText = before <> c <> after
       , cursorPosition = os.cursorPosition + 1
       }
  MoveRight -> \os ->
    if os.cursorPosition <= String.length os.plainText
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
  MoveOneWordRight -> \os ->
    if os.cursorPosition <= String.length os.plainText
      then do
        let idx = findFirstTailFrom os.cursorPosition os.plainText
        os { cursorPosition = idx
           , escapes = aggregateMovements [ wrap $ Back $ (String.length os.plainText + 1) - idx ]
           }
      else identity os
  MoveOneWordLeft -> identity
  MoveToTail -> identity
  MoveToHead -> identity
  DoNothing -> identity

