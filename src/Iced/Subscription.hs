module Iced.Subscription (
  batch,
  none,
  Subscription,
) where

import Foreign
import Foreign.C.Types

data NativeSubscription
type Subscription message = Ptr NativeSubscription

foreign import ccall "iced_subscription_none"
  none :: IO (Subscription message)

-- len items
foreign import ccall "iced_subscription_batch"
  subscription_batch :: CUInt -> Ptr (Subscription message) -> IO (Subscription message)

batch :: [IO (Subscription message)] -> IO (Subscription message)
batch list = do
  items <- sequence list
  let len = fromIntegral $ length items
  itemsPtr <- newArray items
  subscription <- subscription_batch len itemsPtr
  free itemsPtr
  pure subscription
