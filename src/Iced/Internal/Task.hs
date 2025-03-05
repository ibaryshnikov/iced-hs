module Iced.Internal.Task where

import Iced.Future

data Task message
  = Perform (Future message)
  | PerformBlocking (IO message)
  | None
