module Iced.Internal.Command where

import Iced.Future

data Command message
  = Perform (Future message)
  | PerformBlocking (IO message)
  | None
