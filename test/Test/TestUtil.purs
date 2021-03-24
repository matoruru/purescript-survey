module Test.TestUtil where

import Prelude

import Data.Foldable (foldl)
import Survey.Operation (evalOperation)
import Survey.Type (Operation, OutputState)

composeOperations :: Array Operation -> OutputState -> OutputState
composeOperations = foldl (>>>) identity <<< map evalOperation
