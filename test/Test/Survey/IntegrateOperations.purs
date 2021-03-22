module Test.Survey.IntegrageOperations where

import Prelude

import Ansi.Codes (EscapeCode(..))
import Control.Monad.Error.Class (class MonadThrow)
import Data.Foldable (foldl)
import Data.Newtype (wrap)
import Data.Show (class Show)
import Effect.Exception (Error)
import Survey.Operation (Operation(..), evalOperation)
import Survey.Type (OutputState)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.TestUtil (composeOperations)

integrateOperations :: forall t1 t2. Monad t1 => MonadThrow Error t2 => SpecT t2 Unit t1 Unit
integrateOperations = describe "Composing multiple operations" do
  describe "Delete a character under the cursor" do
    it "Delete the last character" do
      composeOperations
        [ MoveLeft
        , DeleteUnderCursor
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 5, plainText: "abcd", escapes: [] }

    it "Delete before the last character" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , DeleteUnderCursor
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 4, plainText: "abce", escapes: [ wrap $ Back 1 ] }

    it "Delete the first character" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , DeleteUnderCursor
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "bcde", escapes: [ wrap $ Back 4 ] }

    it "Delete after the first character" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , DeleteUnderCursor
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 2, plainText: "acde", escapes: [ wrap $ Back 3 ] }

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
      composeOperations
        [ MoveLeft
        , DeleteBackward
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 4, plainText: "abce", escapes: [ wrap $ Back 1 ] }

    it "Delete the first character" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , DeleteBackward
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "bcde", escapes: [ wrap $ Back 4 ] }

    it "Delete after the first character" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , DeleteBackward
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 2, plainText: "acde", escapes: [ wrap $ Back 3 ] }

    it "Deleting backward at 1 should do nothing" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , DeleteBackward
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "abcde", escapes: [ wrap $ Back 5 ] }

  describe "Delete from under the cursor to the end of line" do
    it "Delete the c to the end" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , DeleteUnderCursorToTail
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 3, plainText: "ab", escapes: [] }

    it "Delete from the head" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , DeleteUnderCursorToTail
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "", escapes: [] }

    it "Deleting the last character" do
      composeOperations
        [ MoveLeft
        , DeleteUnderCursorToTail
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
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
      composeOperations
        [ MoveLeft
        , (PrintCharacter "c")
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 6, plainText: "abcdce", escapes: [ wrap $ Back 1 ] }

    it "Print a character from the beginning" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , (PrintCharacter "z")
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 2, plainText: "zabcde", escapes: [ wrap $ Back 5 ] }

    it "Print a character after the first character" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , (PrintCharacter "z")
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 3, plainText: "azbcde", escapes: [ wrap $ Back 4 ] }

  describe "The cursor movements should be aggregated" do
    it "Move left, left, right, and right = []" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveRight
        , MoveRight
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 6, plainText: "abcde", escapes: [] }

    it "Move left, left, left, left and left = [Back 5]" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "abcde", escapes: [ wrap $ Back 5 ] }

    it "Move left, left, right, right and left = [Back 1]" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveRight
        , MoveRight
        , MoveLeft
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 5, plainText: "abcde", escapes: [ wrap $ Back 1 ] }

  describe "MoveToTail" do
    it "" $ fail "TODO"

  describe "MoveToHead" do
    it "" $ fail "TODO"
