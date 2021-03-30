module Test.Survey.Util where

import Prelude

import Ansi.Codes (EscapeCode(..))
import Control.Monad.Error.Class (class MonadThrow)
import Data.Newtype (wrap)
import Data.Show (class Show)
import Effect.Exception (Error)
import Survey.Operation (evalOperation)
import Survey.Util (aggregateMovements, findFirstHeadFrom, findFirstTailFrom)
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

  --
  --  " _      word   "
  --    ^          ^
  --    |          |
  -- position   find here
  --
  describe "Find the first tail of word" do
    it "Only one word" do
      findFirstTailFrom 1 "abc" `shouldEqual` 4

    it "Before the word" do
      findFirstTailFrom 1 "  abc " `shouldEqual` 6

    it "From inside the word" do
      findFirstTailFrom 4 "  abc " `shouldEqual` 6

    it "From end of the word" do
      findFirstTailFrom 5 "  abc " `shouldEqual` 6

    it "Two words" do
      findFirstTailFrom 2 "  abc defg " `shouldEqual` 6

    it "Tail to the next tail" do
      findFirstTailFrom 6 "  abc defg " `shouldEqual` 11

    it "After word" do
      findFirstTailFrom 4 "abc  " `shouldEqual` 6

    it "Without word" do
      findFirstTailFrom 1 "   " `shouldEqual` 4

    it "From the end of line shoudn't do anything" do
      findFirstTailFrom 4 "   " `shouldEqual` 4

    it "When there's no characters shoudn't do anything" do
      findFirstTailFrom 1 "" `shouldEqual` 1

  --
  --  " word       _ "
  --    ^          ^
  --    |          |
  -- find here    position
  --
  describe "Find the first head of word" do
    it "Only one word" do
      findFirstHeadFrom 4 "abc" `shouldEqual` 1

    it "After the word" do
      findFirstHeadFrom 6 "  abc " `shouldEqual` 3

    it "From inside the word" do
      findFirstHeadFrom 4 "  abc " `shouldEqual` 3

    it "From beginning of the word" do
      findFirstHeadFrom 3 "  abc " `shouldEqual` 1

    it "Two words" do
      findFirstHeadFrom 11 "  abc defg " `shouldEqual` 7

    it "Head to the next Head" do
      findFirstHeadFrom 7 "  abc defg " `shouldEqual` 3

    it "Without word" do
      findFirstHeadFrom 4 "   " `shouldEqual` 1

    it "From the beginning of line shoudn't do anything" do
      findFirstHeadFrom 1 "   " `shouldEqual` 1

    it "When there's no characters shoudn't do anything" do
      findFirstHeadFrom 1 "" `shouldEqual` 1
