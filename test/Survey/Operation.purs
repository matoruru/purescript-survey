module Test.Survey.Operation where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)
import Survey.Operation (Operation(..), evalOperation)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)

operations :: forall t1 t2. Monad t1 => MonadThrow Error t2 => SpecT t2 Unit t1 Unit
operations = describe "Test.Survey.Operation" do
 describe "Delete backward from the cursor position" do
   it "delete the last character" do
     evalOperation (DeleteBackward 5) "abcde" `shouldEqual` "abcd"
   it "delete before the last character" do
     evalOperation (DeleteBackward 4) "abcde" `shouldEqual` "abce"
   it "delete the first character" do
     evalOperation (DeleteBackward 1) "abcde" `shouldEqual` "bcde"
   it "delete after the first character" do
     evalOperation (DeleteBackward 2) "abcde" `shouldEqual` "acde"
   it "deleting backward at 0 should do nothing" do
     evalOperation (DeleteBackward 0) "abcde" `shouldEqual` "abcde"
