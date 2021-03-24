module Survey.Util where

import Prelude

import Ansi.Codes (EscapeCode(..))
import Control.Monad.State (runState)
import Control.Monad.State as State
import Data.Array (splitAt)
import Data.Foldable (foldl)
import Data.Newtype (unwrap, wrap)
import Data.String.Utils (toCharArray)
import Data.Tuple (fst)
import Survey.Internal (EscapeCodeWrapper)

aggregateMovements :: Array EscapeCodeWrapper -> Array EscapeCodeWrapper
aggregateMovements wrappers = do
  let { forward, back } = foldl sumUp zero <<< map unwrap $ wrappers
  case forward `compare` back of
    LT -> [ wrap $ Back $ back - forward ]
    GT -> [ wrap $ Forward $ forward - back ]
    EQ -> []
  where
    sumUp :: { forward :: Int, back :: Int } -> _
    sumUp total (Forward n) = total { forward = total.forward + n }
    sumUp total (Back n) = total { back = total.back + n }
    sumUp total _ = total

findFirstTailFrom :: Int -> String -> Int
findFirstTailFrom position str = fst $ flip runState { position, rest: toCharArray str }
  do
    skipTillPosition
    skipWhitespaces
    skipChars
    State.gets _.position
  where
    skipTillPosition = do
      s <- State.get
      State.modify_ _ { rest = (splitAt (s.position - 1) s.rest).after }

    skipWhitespaces = do
      s <- State.get
      case splitAt 1 s.rest of
        { before: [" "], after } -> do
          State.modify_ _ { position = s.position + 1, rest = after }
          skipWhitespaces
        _ -> pure unit

    skipChars = do
      s <- State.get
      case splitAt 1 s.rest of
        { before: [c], after } -> do
          when (c /= " ") do
            State.modify_ _ { position = s.position + 1, rest = after }
            skipChars
        _ -> pure unit
