module Test.Survey.QuestionItem where

import Prelude

import Ansi.Codes (EscapeCode)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Show (class Show)
import Effect.Exception (Error)
import Survey.Operation (Operation(..), evalOperation)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Survey.QuestionItem.Text as QuestionItem

questionItems :: forall t1 t2. Monad t1 => MonadThrow Error t2 => SpecT t2 Unit t1 Unit
questionItems = do
  QuestionItem.text
