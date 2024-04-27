module Iced.Command (
--  perform,
--  performIO,
  Command(..),
) where

import Iced.Future

data Command message
  = Perform (Future message)
  | PerformIO (IO message)
  | None

--perform :: Future -> Command message
--perform future = Perform future
--
--performIO :: IO (message) -> Command message
--performIO io = PerformIO io
