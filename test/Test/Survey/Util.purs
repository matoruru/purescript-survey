module Test.Survey.Util where

import Prelude

import Ansi.Codes (EscapeCode(..))
import Control.Monad.Error.Class (class MonadThrow)
import Data.Newtype (wrap)
import Data.Show (class Show)
import Effect.Exception (Error)
import Survey.Operation (Operation(..), evalOperation)
import Survey.Util (aggregateMovements)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)

utils :: forall t1 t2. Monad t1 => MonadThrow Error t2 => SpecT t2 Unit t1 Unit
utils = do
  describe "Aggregates the movenents array" do
    it "[] = []" do
      aggregateMovements [] `shouldEqual` []

    it "[Forward 1] = [Forward 1]" do
      aggregateMovements [ wrap $ Forward 1 ] `shouldEqual` [ wrap $ Forward 1 ]

    it "[Back 1] = [Back 1]" do
      aggregateMovements [ wrap $ Back 1 ] `shouldEqual` [ wrap $ Back 1 ]

    it "[Forward 3, Back 1] = [Forward 2]" do
      aggregateMovements [ wrap $ Forward 3, wrap $ Back 1 ] `shouldEqual` [ wrap $ Forward 2 ]

    it "[Forward 3, Back 5, Back 1, Forward 4] = [Forward 1]" do
      aggregateMovements
        (map wrap
          [ Forward 3
          , Back 5
          , Back 1
          , Forward 4
          ]) `shouldEqual`
          [ wrap $ Forward 1 ]

    it "[Forward 3, Back 5, Back 1, Forward 4, Back 5] = [Back 4]" do
      aggregateMovements
        (map wrap
          [ Forward 3
          , Back 5
          , Back 1
          , Forward 4
          , Back 5
          ]) `shouldEqual`
          [ wrap $ Back 4 ]
