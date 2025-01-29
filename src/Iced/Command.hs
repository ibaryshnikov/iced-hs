module Iced.Command (
  perform,
  performBlocking,
  none,
  Command,
) where

import Iced.Future
import Iced.Internal.Command

perform :: Future message -> Command message
perform = Perform

performBlocking :: IO message -> Command message
performBlocking = PerformBlocking

none :: Command message
none = None
