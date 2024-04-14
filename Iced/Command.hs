module Iced.Command (
--  perform,
--  performIO,
  Command(..),
) where

import Iced.Time

data Command message
  = Perform (Sleep message) -- the type will change
  | PerformIO (IO message)
  | None

--perform :: Future -> Command message
--perform future = Perform future
--
--performIO :: IO (message) -> Command message
--performIO io = PerformIO io
