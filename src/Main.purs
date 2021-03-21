module Main where

import Prelude

import Ansi.Codes (EraseParam(..), EscapeCode(..), eraseParamToString, escapeCodeToString)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.Traversable (for, sequence)
import Effect (Effect)
import Effect.AVar (AVar, new, put, take, tryPut, tryTake)
import Effect.Aff (Aff, launchAff_, makeAff)
import Effect.Class.Console as Console
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn2, runEffectFn1, runEffectFn2)
import Node.Encoding (Encoding(..))
import Node.Process (stdin, stdout)
import Node.ReadLine (close, createInterface)
import Node.Stream (Readable, writeString)
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
  state <- new { cursorPosition: 0, plainText: "", escapes: [] }
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
            print $ escapeCodeToString (EraseLine Entire) <> escapeCodeToString (HorizontalAbsolute 0)
            let newState = text key os
            _ <- tryPut newState state
            print $ newState.plainText <> joinWith "" (map escapeCodeToString (map unwrap newState.escapes))

            -- Debug
            print $ escapeCodeToString SavePosition
                 <> escapeCodeToString (Position 1 0)
                 <> escapeCodeToString (EraseLine Entire)
                 <> show key
                 <> escapeCodeToString (Position 2 0)
                 <> escapeCodeToString (EraseLine Entire)
                 <> show ((\{ cursorPosition, plainText } -> { cursorPosition, plainText }) newState)
                 <> escapeCodeToString (Position 3 0)
                 <> escapeCodeToString (EraseLine Entire)
                 -- <> show { ctrl: false, meta: false, name: (Just "backspace"), sequence: "\x7F", shift: false }
                 <> escapeCodeToString RestorePosition

  mempty

print :: String -> Effect Unit
print s = void $ flip (writeString stdout UTF8) mempty s

foreign import setRawModeImpl :: EffectFn2 (Readable ()) Boolean Unit
foreign import emitKeypressEventsImpl :: EffectFn1 (Readable ()) Unit
foreign import onKeypressImpl :: (String -> Maybe String) -> Maybe String -> EffectFn2 (Readable ()) KeypressEventHandlerImpl Unit
foreign import removeAllListenersImpl :: EffectFn1 (Readable ()) Unit

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
