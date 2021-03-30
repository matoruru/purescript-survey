module Test.Survey.Operation where

import Prelude

import Ansi.Codes (EscapeCode(..))
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array ((..))
import Data.Foldable (foldl)
import Data.Newtype (wrap)
import Data.Show (class Show)
import Effect.Exception (Error)
import Survey.Operation (evalOperation)
import Survey.Type (Operation(..), OutputState)
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

  describe "Delete a word before the cursor position" do
    it "Delete a first word before cursor from just after the word" do
      composeOperations
        [ DeleteOneWordBeforeCursor
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "", escapes: [] }

    it "Delete a first word before cursor from after some spaces" do
      composeOperations
        [ DeleteOneWordBeforeCursor
        ]
        { cursorPosition: 7, plainText: "abcde ", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "", escapes: [] }

      composeOperations
        [ DeleteOneWordBeforeCursor
        ]
        { cursorPosition: 8, plainText: "abcde  ", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "", escapes: [] }

      composeOperations
        [ DeleteOneWordBeforeCursor
        ]
        { cursorPosition: 9, plainText: "abcde   ", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "", escapes: [] }

      composeOperations
        [ MoveLeft
        , DeleteOneWordBeforeCursor
        ]
        { cursorPosition: 9, plainText: "abcde   ", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: " ", escapes: [ wrap $ Back 1 ] }

    it "Delete a first word before cursor from inside the word" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , DeleteOneWordBeforeCursor
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
        , DeleteOneWordBeforeCursor
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "abcde", escapes: [ wrap $ Back 5 ] }

    it "Delete a word after some words" do
      composeOperations
        [ DeleteOneWordBeforeCursor
        ]
        { cursorPosition: 10, plainText: "abcde abc", escapes: [] } `shouldEqual`
        { cursorPosition: 7, plainText: "abcde ", escapes: [] }

      composeOperations
        [ DeleteOneWordBeforeCursor
        ]
        { cursorPosition: 11, plainText: "abcde  abc", escapes: [] } `shouldEqual`
        { cursorPosition: 8, plainText: "abcde  ", escapes: [] }

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


  --
  --  " _      word   "
  --    ^          ^
  --    |          |
  -- cursor     find here
  --
  describe "Move to right till the end of the word" do
    it "Move from the head without space" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveOneWordRight
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 6, plainText: "abcde", escapes: [] }

    it "Move from the middle without space" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveOneWordRight
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 6, plainText: "abcde", escapes: [] }

    it "Move from the head with spaces" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveOneWordRight
        ]
        { cursorPosition: 10, plainText: "abcde    ", escapes: [] } `shouldEqual`
        { cursorPosition: 6, plainText: "abcde    ", escapes: [ wrap $ Back 4 ] }

      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveOneWordRight
        ]
        { cursorPosition: 13, plainText: "   abcde    ", escapes: [] } `shouldEqual`
        { cursorPosition: 9, plainText: "   abcde    ", escapes: [ wrap $ Back 4 ] }

    it "Move from the head with multiple words" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveOneWordRight
        ]
        { cursorPosition: 13, plainText: "abcde abc aa", escapes: [] } `shouldEqual`
        { cursorPosition: 10, plainText: "abcde abc aa", escapes: [ wrap $ Back 3 ] }

      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveOneWordRight
        ]
        { cursorPosition: 13, plainText: "abcde abc aa", escapes: [] } `shouldEqual`
        { cursorPosition: 10, plainText: "abcde abc aa", escapes: [ wrap $ Back 3 ] }

      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveOneWordRight
        ]
        { cursorPosition: 14, plainText: "abcde abc  aa", escapes: [] } `shouldEqual`
        { cursorPosition: 10, plainText: "abcde abc  aa", escapes: [ wrap $ Back 4 ] }

    it "Move to right but only spaces" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveOneWordRight
        ]
        { cursorPosition: 5, plainText: "    ", escapes: [] } `shouldEqual`
        { cursorPosition: 5, plainText: "    ", escapes: [] }

    it "When there's no character it shouldn't do anything" do
      composeOperations
        [ MoveOneWordRight
        ]
        { cursorPosition: 1, plainText: "", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "", escapes: [] }

  describe "Move to left till the head of the word" do
    it "Move from the tail without space" do
      composeOperations
        [ MoveOneWordLeft
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "abcde", escapes: [ wrap $ Back 5 ] }

    it "Move from the middle without space" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveOneWordLeft
        ]
        { cursorPosition: 6, plainText: "abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "abcde", escapes: [ wrap $ Back 5 ] }

    it "Move from the tail with spaces" do
      composeOperations
        [ MoveOneWordLeft
        ]
        { cursorPosition: 10, plainText: "abcde    ", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "abcde    ", escapes: [ wrap $ Back 9 ] }

    it "Move from the head to head" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveOneWordLeft
        ]
        { cursorPosition: 14, plainText: "  abcd  abcde", escapes: [] } `shouldEqual`
        { cursorPosition: 3, plainText: "  abcd  abcde", escapes: [ wrap $ Back 11 ] }

    it "Move from the tail with multiple words" do
      composeOperations
        [ MoveOneWordLeft
        ]
        { cursorPosition: 13, plainText: "abcde abc aa", escapes: [] } `shouldEqual`
        { cursorPosition: 11, plainText: "abcde abc aa", escapes: [ wrap $ Back 2 ] }

      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveOneWordLeft
        ]
        { cursorPosition: 13, plainText: "abcde abc aa", escapes: [] } `shouldEqual`
        { cursorPosition: 7, plainText: "abcde abc aa", escapes: [ wrap $ Back 6 ] }

    it "Move to left but only spaces" do
      composeOperations
        [ MoveLeft
        , MoveOneWordLeft
        ]
        { cursorPosition: 5, plainText: "    ", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "    ", escapes: [ wrap $ Back 4 ] }

    it "When there's no character it shouldn't do anything" do
      composeOperations
        [ MoveOneWordLeft
        ]
        { cursorPosition: 1, plainText: "", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "", escapes: [] }

  describe "Move to tail" do
    it "Move to tail" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveToTail
        ]
        { cursorPosition: 13, plainText: "abcde abc aa", escapes: [] } `shouldEqual`
        { cursorPosition: 13, plainText: "abcde abc aa", escapes: [] }

    it "Move to tail from head" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveToTail
        ]
        { cursorPosition: 10, plainText: "abcde    ", escapes: [] } `shouldEqual`
        { cursorPosition: 10, plainText: "abcde    ", escapes: [] }

    it "Moving from the tail shouldn't do anything" do
      composeOperations
        [ MoveToTail
        ]
        { cursorPosition: 10, plainText: "abcde    ", escapes: [] } `shouldEqual`
        { cursorPosition: 10, plainText: "abcde    ", escapes: [] }

    it "Moving when without any character shouldn't do anything" do
      composeOperations
        [ MoveToTail
        ]
        { cursorPosition: 1, plainText: "", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "", escapes: [] }

  describe "Move to head" do
    it "Move to tail" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveToHead
        ]
        { cursorPosition: 13, plainText: "abcde abc aa", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "abcde abc aa", escapes: [ wrap $ Back 12 ] }

    it "Move to head from tail" do
      composeOperations
        [ MoveToHead
        ]
        { cursorPosition: 10, plainText: "abcde    ", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "abcde    ", escapes: [ wrap $ Back 9 ] }

    it "Moving from the head shouldn't do anything" do
      composeOperations
        [ MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveLeft
        , MoveToHead
        ]
        { cursorPosition: 10, plainText: "abcde    ", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "abcde    ", escapes: [ wrap $ Back 9 ] }

    it "Moving when without any character shouldn't do anything" do
      composeOperations
        [ MoveToHead
        ]
        { cursorPosition: 1, plainText: "", escapes: [] } `shouldEqual`
        { cursorPosition: 1, plainText: "", escapes: [] }
