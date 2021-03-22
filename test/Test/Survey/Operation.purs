module Test.Survey.Operation where

import Prelude

import Ansi.Codes (EscapeCode(..))
import Control.Monad.Error.Class (class MonadThrow)
import Data.Newtype (wrap)
import Data.Show (class Show)
import Effect.Exception (Error)
import Survey.Operation (Operation(..), evalOperation)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

operations :: forall t1 t2. Monad t1 => MonadThrow Error t2 => SpecT t2 Unit t1 Unit
operations = do
  describe "Delete a character under the cursor" do
    it "Delete the last character" do
      evalOperation DeleteUnderCursor
        { cursorPosition: 5, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 5, plainText: "abcd", escapes: [] }

    it "Delete before the last character" do
      evalOperation DeleteUnderCursor
        { cursorPosition: 4, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 4, plainText: "abce", escapes: [] }

    it "Delete the first character" do
      evalOperation DeleteUnderCursor
        { cursorPosition: 1, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "bcde", escapes: [] }

    it "Delete after the first character" do
      evalOperation DeleteUnderCursor
        { cursorPosition: 2, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 2, plainText: "acde", escapes: [] }

    it "Deleting at the end of line shouldn't do anything" do
      evalOperation DeleteUnderCursor
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 6, plainText: "abcde", escapes: [] }

  describe "Delete backward from the cursor position" do
    it "Delete the last character" do
      evalOperation DeleteBackward
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 5, plainText: "abcd", escapes: [] }

    it "Delete before the last character" do
      evalOperation DeleteBackward
        { cursorPosition: 5, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 4, plainText: "abce", escapes: [] }

    it "Delete the first character" do
      evalOperation DeleteBackward
        { cursorPosition: 2, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "bcde", escapes: [] }

    it "Delete after the first character" do
      evalOperation DeleteBackward
        { cursorPosition: 3, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 2, plainText: "acde", escapes: [] }

    it "Deleting backward at 1 shoudn't do anything" do
      evalOperation DeleteBackward
        { cursorPosition: 1, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "abcde", escapes: [] }

  describe "Delete from under the cursor to the end of line" do
    it "Delete the c to the end" do
      evalOperation DeleteUnderCursorToTail
        { cursorPosition: 3, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 3, plainText: "ab", escapes: [] }

    it "Delete from the head" do
      evalOperation DeleteUnderCursorToTail
        { cursorPosition: 1, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "", escapes: [] }

    it "Deleting the last character" do
      evalOperation DeleteUnderCursorToTail
        { cursorPosition: 5, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 5, plainText: "abcd", escapes: [] }

    it "Deleting from the end shoudn't do anything" do
      evalOperation DeleteUnderCursorToTail
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 6, plainText: "abcde", escapes: [] }

  describe "DeleteBeforeUnderCursorToHead" do
    it "" $ fail "TODO"

  describe "DeleteAWordBeforeCursor" do
    it "" $ fail "TODO"

  describe "Print a character from the cursor position" do
    it "Print a character from the end" do
      evalOperation (PrintCharacter "f")
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 7, plainText: "abcdef", escapes: [] }

    it "Print a character before the last character" do
      evalOperation (PrintCharacter "c")
        { cursorPosition: 5, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 6, plainText: "abcdce", escapes: [] }

    it "Print a character from the beginning" do
      evalOperation (PrintCharacter "z")
        { cursorPosition: 1, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 2, plainText: "zabcde", escapes: [] }

    it "Print a character after the first character" do
      evalOperation (PrintCharacter "z")
        { cursorPosition: 2, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 3, plainText: "azbcde", escapes: [] }

  describe "Move the cursor" do
    it "Move right, d to e" do
      evalOperation MoveRight
        { cursorPosition: 4, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 5, plainText: "abcde", escapes: [ wrap $ Forward 1 ] }

    it "Move right, e to the tail" do
      evalOperation MoveRight
        { cursorPosition: 5, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 6, plainText: "abcde", escapes: [ wrap $ Forward 1 ] }

    it "Cannot move right from the tail" do
      evalOperation MoveRight
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 6, plainText: "abcde", escapes: [] }

    it "Move left, c to b" do
      evalOperation MoveLeft
        { cursorPosition: 3, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 2, plainText: "abcde", escapes: [ wrap $ Back 1 ] }

    it "Move left, b to a (head)" do
      evalOperation MoveLeft
        { cursorPosition: 2, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "abcde", escapes: [ wrap $ Back 1 ] }

    it "Cannot move left from the head" do
      evalOperation MoveLeft
        { cursorPosition: 1, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "abcde", escapes: [] }

  describe "MoveToTail" do
    it "" $ fail "TODO"

  describe "MoveToHead" do
    it "" $ fail "TODO"
