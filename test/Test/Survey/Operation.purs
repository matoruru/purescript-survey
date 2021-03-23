module Test.Survey.Operation where

import Prelude

import Ansi.Codes (EscapeCode(..))
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array ((..))
import Data.Foldable (foldl)
import Data.Newtype (wrap)
import Data.Show (class Show)
import Effect.Exception (Error)
import Survey.Operation (Operation(..), evalOperation)
import Survey.Type (OutputState)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.TestUtil (composeOperations)

operations :: forall t1 t2. Monad t1 => MonadThrow Error t2 => SpecT t2 Unit t1 Unit
operations = describe "Composing multiple operations" do
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

  describe "Delete from before the cursor to the head of line" do
    it "Delete the first character" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , DeleteBeforeUnderCursorToHead
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "bcde", escapes: [ wrap $ Back 4 ] }

    it "Delete the first two characters" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , DeleteBeforeUnderCursorToHead
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "cde", escapes: [ wrap $ Back 3 ] }

    it "Delete from the end of line (delete all characters)" do
      composeOperations
        [ DeleteBeforeUnderCursorToHead
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "", escapes: [] }

    it "Delete from the head shoudn't do anything" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , DeleteBeforeUnderCursorToHead
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "abcde", escapes: [ wrap $ Back 5 ] }

  describe "DeleteAWordBeforeCursor" do
    it "Delete a first word before cursor from just after the word" do
      composeOperations
        [ DeleteAWordBeforeCursor
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "", escapes: [] }

    it "Delete a first word before cursor from after some spaces" do
      composeOperations
        [ DeleteAWordBeforeCursor
        ]
        { cursorPosition: 7, plainText: "abcde ", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "", escapes: [] }

    it "Delete a first word before cursor from after some spaces" do
      composeOperations
        [ DeleteAWordBeforeCursor
        ]
        { cursorPosition: 8, plainText: "abcde  ", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "", escapes: [] }

    it "Delete a first word before cursor from after some spaces" do
      composeOperations
        [ DeleteAWordBeforeCursor
        ]
        { cursorPosition: 9, plainText: "abcde   ", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "", escapes: [] }

    it "Delete a first word before cursor from after some spaces" do
      composeOperations
        [ DeleteAWordBeforeCursor
        ]
        { cursorPosition: 8, plainText: "abcde   ", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: " ", escapes: [ wrap $ Back 1 ] }

    it "Delete a first word before cursor from inside the word" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , DeleteAWordBeforeCursor
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "de", escapes: [ wrap $ Back 2 ] }

    it "Deleting a first word from the head shoudn't do anything" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , DeleteAWordBeforeCursor
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "abcde", escapes: [ wrap $ Back 5 ] }

    it "Delete a word after some words" do
      composeOperations
        [ DeleteAWordBeforeCursor
        ]
        { cursorPosition: 10, plainText: "abcde abc", escapes: [] } `shouldEqual`
        { cursorPosition: 7, plainText: "abcde ", escapes: [] }

    it "Delete a word after some words" do
      composeOperations
        [ DeleteAWordBeforeCursor
        ]
        { cursorPosition: 11, plainText: "abcde  abc", escapes: [] } `shouldEqual`
        { cursorPosition: 8, plainText: "abcde  ", escapes: [] }

    it "Delete a word separated with \"/\"" do
      composeOperations
        [ DeleteAWordBeforeCursor
        ]
        { cursorPosition: 10, plainText: "abcde/abc", escapes: [] } `shouldEqual`
        { cursorPosition: 7, plainText: "abcde/", escapes: [] }

    it "Delete a word separated with \"/\" from inside the word" do
      composeOperations
        [ DeleteAWordBeforeCursor
        ]
        { cursorPosition: 8, plainText: "abcde/abc", escapes: [] } `shouldEqual`
        { cursorPosition: 8, plainText: "abcde/bc", escapes: [ wrap $ Back 2 ] }

    it "Delete the first \"/\"" do
      composeOperations
        [ DeleteAWordBeforeCursor
        ]
        { cursorPosition: 2, plainText: "/", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "", escapes: [] }

    it "Delete the first \"/\" spaces" do
      composeOperations
        [ DeleteAWordBeforeCursor
        ]
        { cursorPosition: 4, plainText: "/  ", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "", escapes: [] }

    it "Delete the first \"/\" spaces" do
      composeOperations
        [ DeleteAWordBeforeCursor
        ]
        { cursorPosition: 5, plainText: " /  ", escapes: [] } `shouldEqual`
        { cursorPosition: 2, plainText: " ", escapes: [] }

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
