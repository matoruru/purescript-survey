module Main where

import Prelude

import Ansi.Codes (EraseParam(..), EscapeCode(..), eraseParamToString, escapeCodeToString)
import Data.Array (concat, (..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (joinWith, length)
import Data.Traversable (for, sequence)
import Effect (Effect)
import Effect.AVar (AVar, new, put, take, tryPut, tryTake)
import Effect.Aff (Aff, launchAff_, makeAff)
import Effect.Class.Console as Console
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn2, runEffectFn1, runEffectFn2)
import Math ((%))
import Node.Encoding (Encoding(..))
import Node.Process (stdin, stdout)
import Node.ReadLine (close, createInterface)
import Node.Stream (Readable, Writable, writeString)
import Survey.Internal (Key)
import Survey.QuestionItem.Text (text)
import Unsafe.Coerce (unsafeCoerce)

type KeypressEventHandlerImpl = EffectFn2 String Key Unit

type KeypressEventHandler = String -> Key -> Effect Unit

main :: Effect Unit
main = launchAff_ do
  keypressLoop

keypressLoop :: Aff Unit
keypressLoop = makeAff \cb -> do
  emitKeypressEvents stdin
  when stdinIsTTY do
    setRawMode stdin true
  interface <- createInterface stdin mempty
  state <- new { cursorPosition: 1, plainText: "", escapes: [] }
  onKeypress stdin $ \_ key -> do
    outputState <- tryTake state
    case key.name of
      Just "q" -> do
        setRawMode stdin false
        close interface
        removeAllListeners stdin
        cb $ pure unit
      _ -> do
        case outputState of
          Nothing -> pure unit
          Just os -> do
            columns' <- columns stdout
            let newState = text key os
            let lines = (length newState.plainText - 1) / columns' + 1
            print $ escapeCodeToString (EraseLine Entire) <> escapeCodeToString (HorizontalAbsolute 0)
            if lines == 2 then do
              print $ escapeCodeToString $ Up 1
            else pure unit
            _ <- tryPut newState state
            print $ newState.plainText <> joinWith "" (map escapeCodeToString (map unwrap newState.escapes))
            if (toNumber $ newState.cursorPosition) % (toNumber columns') == 1.0
              then print newLineCharacter
              else pure unit

            -- Debug
            print $ escapeCodeToString SavePosition
                 <> escapeCodeToString (Position 1 0)
                 <> escapeCodeToString (EraseLine Entire)
                 <> show key
                 <> escapeCodeToString (Position 2 0)
                 <> escapeCodeToString (EraseLine Entire)
                 <> show newState
                 <> escapeCodeToString (Position 3 0)
                 <> escapeCodeToString (EraseLine Entire)
                 <> show { ctrl: false, meta: false, name: (Just "backspace"), sequence: "\x1B[D1;5D", shift: false }
                 <> escapeCodeToString (Position 4 0)
                 <> escapeCodeToString (EraseLine Entire)
                 <> "columns: " <> show columns'
                 <> escapeCodeToString (Position 5 0)
                 <> escapeCodeToString (EraseLine Entire)
                 <> "lines: " <> show lines
                 <> escapeCodeToString RestorePosition

  mempty

newLineCharacter :: String
newLineCharacter =  """
"""

print :: String -> Effect Unit
print s = void $ flip (writeString stdout UTF8) mempty s

foreign import setRawModeImpl :: EffectFn2 (Readable ()) Boolean Unit
foreign import emitKeypressEventsImpl :: EffectFn1 (Readable ()) Unit
foreign import onKeypressImpl :: (String -> Maybe String) -> Maybe String -> EffectFn2 (Readable ()) KeypressEventHandlerImpl Unit
foreign import removeAllListenersImpl :: EffectFn1 (Readable ()) Unit
foreign import columnsImpl :: EffectFn1 (Writable ()) Int

setRawMode :: Readable () -> Boolean -> Effect Unit
setRawMode = runEffectFn2 setRawModeImpl

emitKeypressEvents :: Readable () -> Effect Unit
emitKeypressEvents = runEffectFn1 emitKeypressEventsImpl

onKeypress :: Readable () -> KeypressEventHandler -> Effect Unit
onKeypress stream handler = runEffectFn2 (onKeypressImpl Just Nothing) stream (mkEffectFn2 handler)

removeAllListeners :: Readable () -> Effect Unit
removeAllListeners = runEffectFn1 removeAllListenersImpl

stdinIsTTY :: Boolean
stdinIsTTY = (unsafeCoerce stdin).isTTY

columns :: Writable () -> Effect Int
columns = runEffectFn1 columnsImpl
