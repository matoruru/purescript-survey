module Test.Survey.IntegrageOperations where

import Prelude

import Ansi.Codes (EscapeCode(..))
import Control.Monad.Error.Class (class MonadThrow)
import Data.Newtype (wrap)
import Data.Show (class Show)
import Effect.Exception (Error)
import Survey.Operation (Operation(..), evalOperation)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)

integrateOperations :: forall t1 t2. Monad t1 => MonadThrow Error t2 => SpecT t2 Unit t1 Unit
integrateOperations = describe "Test.Survey.IntegrageOperations" do
  describe "Delete backward from the cursor position" do
    it "Delete the last character" do
      evalOperation DeleteBackward
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 5, plainText: "abcd", escapes: [] }

    it "Delete before the last character" do
      ( evalOperation MoveLeft
      >>> evalOperation DeleteBackward
      )
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 4, plainText: "abce", escapes: [ wrap $ Back 1 ] }

    it "Delete the first character" do
      ( evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation DeleteBackward
      )
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "bcde", escapes: [ wrap $ Back 4 ] }

    it "Delete after the first character" do
      ( evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation DeleteBackward
      )
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 2, plainText: "acde", escapes: [ wrap $ Back 3 ] }

    it "Deleting backward at 1 should do nothing" do
      ( evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation DeleteBackward
      )
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "abcde", escapes: [ wrap $ Back 5 ] }

  describe "Print a character from the cursor position" do
    it "Print a character from the end" do
      evalOperation (PrintCharacter "f")
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 7, plainText: "abcdef", escapes: [] }

    it "Print a character before the last character" do
      ( evalOperation MoveLeft
      >>> evalOperation (PrintCharacter "c")
      )
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 6, plainText: "abcdce", escapes: [ wrap $ Back 1 ] }

    it "Print a character from the beginning" do
      ( evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation (PrintCharacter "z")
      )
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 2, plainText: "zabcde", escapes: [ wrap $ Back 5 ] }

    it "Print a character after the first character" do
      ( evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation (PrintCharacter "z")
      )
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 3, plainText: "azbcde", escapes: [ wrap $ Back 4 ] }

  describe "Move the cursor" do
    it "Move right, d to e" do
      ( evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveRight
      )
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 5, plainText: "abcde", escapes: [ wrap $ Back 1 ] }

    it "Move right, e to the tail" do
      ( evalOperation MoveLeft
      >>> evalOperation MoveRight
      )
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 6, plainText: "abcde", escapes: [] }

    it "Cannot move right from the tail" do
      evalOperation MoveRight
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 6, plainText: "abcde", escapes: [] }

    it "Move left, c to b" do
      ( evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveLeft
      )
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 2, plainText: "abcde", escapes: [ wrap $ Back 4 ] }

    it "Move left, b to a (head)" do
      ( evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveLeft
      )
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "abcde", escapes: [ wrap $ Back 5 ] }

    it "Cannot move left from the head" do
      ( evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveLeft
      )
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "abcde", escapes: [ wrap $ Back 5 ] }

  describe "The cursor movements should be aggregated" do
    it "Move right, d to the tail" do
      ( evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveRight
      >>> evalOperation MoveRight
      )
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 6, plainText: "abcde", escapes: [] }

    it "Move left, c to a (the head)" do
      ( evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveLeft
      )
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "abcde", escapes: [ wrap $ Back 5 ] }

    it "Move right, right and left" do
      ( evalOperation MoveLeft
      >>> evalOperation MoveLeft
      >>> evalOperation MoveRight
      >>> evalOperation MoveRight
      >>> evalOperation MoveLeft
      )
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 5, plainText: "abcde", escapes: [ wrap $ Back 1 ] }
