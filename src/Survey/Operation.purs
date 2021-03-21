module Survey.Operation where

import Prelude

import Data.String (splitAt)
import Survey.Type (CursorPosition)

data Operation
  = DeleteBackward CursorPosition
  | DeleteForward CursorPosition

evalOperation :: Operation -> (String -> String)
evalOperation = case _ of
  DeleteBackward position -> \s -> (splitAt (position - 1) s).before <> (splitAt position s).after
  DeleteForward position -> \v -> v
