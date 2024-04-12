module Iced.Command where

-- placeholder for a future Command api
data Command message = Perform (IO (message)) | None
