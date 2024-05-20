module Iced.Subscription (
  Subscription,
  none,
) where

import Foreign

data NativeSubscription
type Subscription message = Ptr NativeSubscription

foreign import ccall "iced_subscription_none"
  none :: IO (Subscription message)
