module Main where

import Prelude

import Data.Options ((:=))
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn2, runEffectFn1, runEffectFn2)
import Node.Process (stdin, stdout)
import Node.ReadLine (Interface, close, createInterface, output)
import Node.Stream (Readable)
import Unsafe.Coerce (unsafeCoerce)

type KeypressEventHandlerImpl = EffectFn2 String Key Unit

type KeypressEventHandler = String -> Key -> Effect Unit

type Key =
  { sequence :: String
  , name     :: String
  , ctrl     :: Boolean
  , meta     :: Boolean
  , shift    :: Boolean
  }

foreign import setRawModeImpl :: EffectFn2 (Readable ()) Boolean Unit
foreign import emitKeypressEventsImpl :: EffectFn1 (Readable ()) Unit
foreign import onKeypressImpl :: EffectFn2 (Readable ()) KeypressEventHandlerImpl Unit
foreign import removeAllListenersImpl :: EffectFn1 (Readable ()) Unit

setRawMode :: Readable () -> Boolean -> Effect Unit
setRawMode = runEffectFn2 setRawModeImpl

emitKeypressEvents :: Readable () -> Effect Unit
emitKeypressEvents = runEffectFn1 emitKeypressEventsImpl

onKeypress :: Readable () -> KeypressEventHandler -> Effect Unit
onKeypress stream handler = runEffectFn2 onKeypressImpl stream (mkEffectFn2 handler)

removeAllListeners :: Readable () -> Effect Unit
removeAllListeners = runEffectFn1 removeAllListenersImpl

stdinIsTTY :: Boolean
stdinIsTTY = (unsafeCoerce stdin).isTTY

main :: Effect Unit
main = do
  emitKeypressEvents stdin
  when stdinIsTTY do
    setRawMode stdin true
    interface <- createInterface stdin mempty
    onKeypress stdin $ keypressHandler interface

keypressHandler :: Interface -> KeypressEventHandler
keypressHandler interface _ key = case key.name of
  "return" -> do
    setRawMode stdin false
    close interface
    removeAllListeners stdin
  _ -> Console.logShow key
