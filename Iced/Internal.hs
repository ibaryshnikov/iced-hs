module Iced.Internal where

import Foreign

foreign export ccall "free_stable_ptr"
  freeStablePtr :: StablePtr a -> IO ()
