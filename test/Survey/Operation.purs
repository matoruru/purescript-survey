module Test.Survey.Operation where

import Prelude

import Ansi.Codes (EscapeCode)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Show (class Show)
import Effect.Exception (Error)
import Survey.Operation (Operation(..), evalOperation)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)

operations :: forall t1 t2. Monad t1 => MonadThrow Error t2 => SpecT t2 Unit t1 Unit
operations = describe "Test.Survey.Operation" do
 -- To avoid lacking Show instance for escapes
 let removeEscapes = \{ cursorPosition, plainText } -> { cursorPosition, plainText }

 describe "Delete backward from the cursor position" do
   it "delete the last character" do
     removeEscapes (evalOperation DeleteBackward
       { cursorPosition: 5, plainText: "abcde", escapes: [] }) `shouldEqual`
       { cursorPosition: 4, plainText: "abcd" }

   it "delete before the last character" do
     removeEscapes (evalOperation DeleteBackward
       { cursorPosition: 4, plainText: "abcde", escapes: [] }) `shouldEqual`
       { cursorPosition: 3, plainText: "abce" }

   it "delete the first character" do
     removeEscapes (evalOperation DeleteBackward
       { cursorPosition: 1, plainText: "abcde", escapes: [] }) `shouldEqual`
       { cursorPosition: 0, plainText: "bcde" }

   it "delete after the first character" do
     removeEscapes (evalOperation DeleteBackward
       { cursorPosition: 2, plainText: "abcde", escapes: [] }) `shouldEqual`
       { cursorPosition: 1, plainText: "acde" }

   it "deleting backward at 0 should do nothing" do
     removeEscapes (evalOperation DeleteBackward
       { cursorPosition: 0, plainText: "abcde", escapes: [] }) `shouldEqual`
       { cursorPosition: 0, plainText: "abcde" }

 describe "Print a character from the cursor position" do
   it "Print a character from the end" do
     removeEscapes (evalOperation (PrintCharacter "f")
       { cursorPosition: 5, plainText: "abcde", escapes: [] }) `shouldEqual`
       { cursorPosition: 6, plainText: "abcdef" }

   it "Print a character before the last character" do
     removeEscapes (evalOperation (PrintCharacter "c")
       { cursorPosition: 4, plainText: "abcde", escapes: [] }) `shouldEqual`
       { cursorPosition: 5, plainText: "abcdce" }

   it "Print a character from the beginning" do
     removeEscapes (evalOperation (PrintCharacter "z")
       { cursorPosition: 0, plainText: "abcde", escapes: [] }) `shouldEqual`
       { cursorPosition: 1, plainText: "zabcde" }

   it "Print a character after the first character" do
     removeEscapes (evalOperation (PrintCharacter "z")
       { cursorPosition: 1, plainText: "abcde", escapes: [] }) `shouldEqual`
       { cursorPosition: 2, plainText: "azbcde" }
