module Survey.Type where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Survey.Internal (EscapeCodeWrapper)

data Operation
  = DeleteUnderCursor
  | DeleteBackward
  | DeleteUnderCursorToTail
  | DeleteBeforeUnderCursorToHead
  | DeleteOneWordBeforeCursor
  | PrintCharacter String
  | MoveRight
  | MoveLeft
  | MoveOneWordRight
  | MoveOneWordLeft
  | MoveToTail
  | MoveToHead
  | DoNothing

derive instance genericOperation :: Generic Operation _

instance showOperation :: Show Operation where
  show = genericShow

instance eqOperation :: Eq Operation where
  eq = genericEq

type CursorPosition = Int

type OutputState =
  { cursorPosition :: CursorPosition
  , plainText :: String
  , escapes :: Array EscapeCodeWrapper
  }
