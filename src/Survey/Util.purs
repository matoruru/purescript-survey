module Survey.Util where

import Prelude

import Ansi.Codes (EscapeCode(..))
import Data.Foldable (foldl)
import Data.Newtype (unwrap, wrap)
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
