module Iced.Internal where

import Foreign

foreign export capi "free_stable_ptr"
  freeStablePtr :: StablePtr a -> IO ()

foreign export capi "free_haskell_fun_ptr"
  freeHaskellFunPtr :: FunPtr a -> IO ()
