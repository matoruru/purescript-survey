module Test.Survey.QuestionItem.Text where

import Prelude

import Ansi.Codes (EscapeCode)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Maybe (Maybe(..))
import Data.Show (class Show)
import Effect.Exception (Error)
import Survey.Operation (Operation(..), evalOperation)
import Survey.QuestionItem.Text (keyToOperation)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

text :: forall t1 t2. Monad t1 => MonadThrow Error t2 => SpecT t2 Unit t1 Unit
text = describe "Test.Survey.QuestionItem.Text" do
  describe "Press key" do
    it "[ctrl]+[h] deletes a character before the cursor" do
      keyToOperation
        -- \x8 = \b
        { ctrl: false, meta: false, name: (Just "backspace"), sequence: "\x8", shift: false } `shouldEqual`
        DeleteBackward

    it "[backspace] deletes a character before the cursor" do
      keyToOperation
        -- \x7F = \127
        { ctrl: false, meta: false, name: (Just "backspace"), sequence: "\x7F", shift: false } `shouldEqual`
        DeleteBackward

    it "[up] and [down] don't do anything" do
      keyToOperation
        -- \x1B[A = \27[A
        { ctrl: false, meta: false, name: (Just "up"), sequence: "\x1B[A", shift: false } `shouldEqual`
        DoNothing

      keyToOperation
        -- \x1B[B = \27[B
        { ctrl: false, meta: false, name: (Just "down"), sequence: "\x1B[B", shift: false } `shouldEqual`
        DoNothing

    it "[right] and [left] moves the cursor to its direction" do
       keyToOperation
         -- \x1B[C = \27[C
         { ctrl: false, meta: false, name: (Just "right"), sequence: "\x1B[C", shift: false } `shouldEqual`
         MoveRight

       keyToOperation
         -- \x1B[D = \27[D
         { ctrl: false, meta: false, name: (Just "left"), sequence: "\x1B[D", shift: false } `shouldEqual`
         MoveLeft

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
