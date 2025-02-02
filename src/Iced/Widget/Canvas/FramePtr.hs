module Iced.Widget.Canvas.FramePtr where

import Foreign

data NativeFrame
type FramePtr = Ptr NativeFrame
