module Main where

import Prelude

import Ansi.Codes (EscapeCode(..), escapeCodeToString)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff)
import Effect.Class.Console as Console
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn2, runEffectFn1, runEffectFn2)
import Node.Encoding (Encoding(..))
import Node.Process (stdin, stdout)
import Node.ReadLine (close, createInterface)
import Node.Stream (Readable, writeString)
import Survey.Internal (Key)
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
  onKeypress stdin $ \_ key -> do
    case key.name of
      Just "return" -> do
        setRawMode stdin false
        close interface
        removeAllListeners stdin
        cb $ pure unit
      --"backspace" -> print $ (escapeCodeToString $ Back 1) <> " " <> (escapeCodeToString $ Back 1)
      --"up" -> print $ escapeCodeToString (Up 1)
      --"s" -> print $ escapeCodeToString SavePosition
      --"r" -> print $ escapeCodeToString RestorePosition
      --"q" -> print $ escapeCodeToString QueryPosition
      s -> print $ show key <> "\n"
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
