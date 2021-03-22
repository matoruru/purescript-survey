module Test.Util where

import Prelude

import Data.Foldable (foldl)
import Survey.Operation (Operation, evalOperation)
import Survey.Type (OutputState)

composeOperations :: Array Operation -> OutputState -> OutputState
composeOperations = foldl (>>>) identity <<< map evalOperation
