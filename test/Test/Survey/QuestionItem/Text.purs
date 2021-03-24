module Test.Survey.QuestionItem.Text where

import Prelude

import Ansi.Codes (EscapeCode)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Char (fromCharCode, toCharCode)
import Data.Maybe (Maybe(..))
import Data.Show (class Show)
import Effect.Exception (Error)
import Survey.Operation (evalOperation)
import Survey.QuestionItem.Text (keyToOperation)
import Survey.Type (Operation(..))
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

text :: forall t1 t2. Monad t1 => MonadThrow Error t2 => SpecT t2 Unit t1 Unit
text = do
  describe "Press key" do
    it "[ctrl]+[h] should delete a character before the cursor" do
      keyToOperation
        -- \x8 = \b
        { ctrl: false, meta: false, name: (Just "backspace"), sequence: "\x8", shift: false } `shouldEqual`
        DeleteBackward

    it "[ctrl]+[b] should move left" do
      keyToOperation
        -- \x2 = \2
        { ctrl: true, meta: false, name: (Just "b"), sequence: "\x2", shift: false } `shouldEqual`
        MoveLeft

    it "[ctrl]+[f] should move right" do
      keyToOperation
        -- \x6 = \6
        { ctrl: true, meta: false, name: (Just "f"), sequence: "\x6", shift: false } `shouldEqual`
        MoveRight

    it "[ctrl]+[d] should delete a character under the cursor" do
      keyToOperation
        -- \x4 = \4
        { ctrl: true, meta: false, name: (Just "d"), sequence: "\x4", shift: false } `shouldEqual`
        DeleteUnderCursor

    it "[ctrl]+[k] should delete the characters under the cursor until the end of the line" do
      keyToOperation
        -- \xb = \v
        { ctrl: true, meta: false, name: (Just "k"), sequence: "\xb", shift: false } `shouldEqual`
        DeleteUnderCursorToTail

    it "[ctrl]+[u] should delete the characters under the cursor until the end of the line" do
      keyToOperation
        -- \x15 = \21
        { ctrl: true, meta: false, name: (Just "u"), sequence: "\x15", shift: false } `shouldEqual`
        DeleteBeforeUnderCursorToHead

    it "[ctrl]+[a] shoule move to the beginning of the line" do
      keyToOperation
        -- \x1 = \1
        { ctrl: true, meta: false, name: (Just "a"), sequence: "\x1", shift: false } `shouldEqual`
        MoveToHead

    it "[ctrl]+[e] shoule move to the end of the line" do
      keyToOperation
        -- \x5 = \5
        { ctrl: true, meta: false, name: (Just "e"), sequence: "\x5", shift: false } `shouldEqual`
        MoveToTail

    it "[ctrl]+[w] should delete from before cursor till the head of word" do
      keyToOperation
        -- \x17 = \23
        { ctrl: true, meta: false, name: (Just "w"), sequence: "\x17", shift: false } `shouldEqual`
        DeleteOneWordBeforeCursor

    it "[space] should prints a space" do
      keyToOperation
        { ctrl: false, meta: false, name: (Just "space"), sequence: " ", shift: false } `shouldEqual`
        PrintCharacter " "

    it "[tab] shouldn't do anything" do
      keyToOperation
        { ctrl: false, meta: false, name: (Just "tab"), sequence: "\t", shift: false } `shouldEqual`
        DoNothing

    it "[backspace] should delete a character before the cursor" do
      keyToOperation
        -- \x7F = \127
        { ctrl: false, meta: false, name: (Just "backspace"), sequence: "\x7F", shift: false } `shouldEqual`
        DeleteBackward

    it "[delete] should delete a character under the cursor" do
      keyToOperation
        -- \x1b[3~ = \27[3~
        { ctrl: false, meta: false, name: (Just "delete"), sequence: "\x1b[3~", shift: false } `shouldEqual`
        DeleteUnderCursor

    it "[up] shouldn't do anything" do
      keyToOperation
        -- \x1B[A = \27[A
        { ctrl: false, meta: false, name: (Just "up"), sequence: "\x1B[A", shift: false } `shouldEqual`
        DoNothing

    it "[down] shouldn't do anything" do
      keyToOperation
        -- \x1B[B = \27[B
        { ctrl: false, meta: false, name: (Just "down"), sequence: "\x1B[B", shift: false } `shouldEqual`
        DoNothing

    it "[right] should move the cursor to right" do
       keyToOperation
         -- \x1B[C = \27[C
         { ctrl: false, meta: false, name: (Just "right"), sequence: "\x1B[C", shift: false } `shouldEqual`
         MoveRight

    it "[left] should move the cursor to left" do
       keyToOperation
         -- \x1B[D = \27[D
         { ctrl: false, meta: false, name: (Just "left"), sequence: "\x1B[D", shift: false } `shouldEqual`
         MoveLeft

    it "[ctrl]+[right] should move the cursor to just after the tail of the next word" do
       keyToOperation
         -- \x1B[1;5C = \27[1;5C
         { ctrl: true, meta: false, name: (Just "right"), sequence: "\x1B[1;5C", shift: false } `shouldEqual`
         MoveOneWordRight

    it "[ctrl]+[left] should move the cursor to the head of the previous word" do
       keyToOperation
         -- \x1B[1;5D = \27[1;5D
         { ctrl: true, meta: false, name: (Just "left"), sequence: "\x1B[D1;5D", shift: false } `shouldEqual`
         MoveOneWordLeft


    it "[a-zA-Z0-9!$@#... ] prints itself" do
      -- https://www.ionos.com/digitalguide/server/know-how/ascii-codes-overview-of-all-characters-on-the-ascii-table/

      keyToOperation
        { ctrl: false, meta: false, name: (Just "a"), sequence: "a", shift: false } `shouldEqual`
        PrintCharacter "a"

      keyToOperation
        { ctrl: false, meta: false, name: (Just "z"), sequence: "z", shift: false } `shouldEqual`
        PrintCharacter "z"

      keyToOperation
        { ctrl: false, meta: false, name: (Just "A"), sequence: "A", shift: true } `shouldEqual`
        PrintCharacter "A"

      keyToOperation
        { ctrl: false, meta: false, name: (Just "Z"), sequence: "Z", shift: true } `shouldEqual`
        PrintCharacter "Z"

      keyToOperation
        { ctrl: false, meta: false, name: (Just "0"), sequence: "0", shift: true } `shouldEqual`
        PrintCharacter "0"

      keyToOperation
        { ctrl: false, meta: false, name: (Just "9"), sequence: "9", shift: true } `shouldEqual`
        PrintCharacter "9"

      keyToOperation
        { ctrl: false, meta: false, name: Nothing, sequence: "!", shift: false } `shouldEqual`
        PrintCharacter "!"

      keyToOperation
        { ctrl: false, meta: false, name: Nothing, sequence: "/", shift: false } `shouldEqual`
        PrintCharacter "/"

      keyToOperation
        { ctrl: false, meta: false, name: Nothing, sequence: ":", shift: false } `shouldEqual`
        PrintCharacter ":"

      keyToOperation
        { ctrl: false, meta: false, name: Nothing, sequence: "@", shift: false } `shouldEqual`
        PrintCharacter "@"

      keyToOperation
        { ctrl: false, meta: false, name: Nothing, sequence: "[", shift: false } `shouldEqual`
        PrintCharacter "["

      keyToOperation
        { ctrl: false, meta: false, name: Nothing, sequence: "`", shift: false } `shouldEqual`
        PrintCharacter "`"

      keyToOperation
        { ctrl: false, meta: false, name: Nothing, sequence: "{", shift: false } `shouldEqual`
        PrintCharacter "{"

      keyToOperation
        { ctrl: false, meta: false, name: Nothing, sequence: "~", shift: false } `shouldEqual`
        PrintCharacter "~"

      keyToOperation
        { ctrl: false, meta: false, name: Nothing, sequence: "~", shift: false } `shouldEqual`
        PrintCharacter "~"
