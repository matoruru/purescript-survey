module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Survey.IntegrageOperations (integrateOperations) as Test
import Test.Survey.Operation (operations) as Test
import Test.Survey.QuestionItem (questionItems) as Test
import Test.Survey.Util (utils) as Test

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  Test.operations
  Test.integrateOperations
  Test.questionItems
  Test.utils
