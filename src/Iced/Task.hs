module Iced.Task (
  perform,
  performBlocking,
  none,
  Task,
) where

import Iced.Future
import Iced.Internal.Task

perform :: Future message -> Task message
perform = Perform

performBlocking :: IO message -> Task message
performBlocking = PerformBlocking

none :: Task message
none = None
