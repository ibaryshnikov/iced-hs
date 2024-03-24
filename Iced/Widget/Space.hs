module Iced.Widget.Space (
  horizontalSpace,
  verticalSpace,
) where

import Iced.Element

foreign import ccall safe "new_horizontal_space"
  new_horizontal_space :: Element
foreign import ccall safe "new_vertical_space"
  new_vertical_space :: Element

horizontalSpace :: Element
horizontalSpace = new_horizontal_space

verticalSpace :: Element
verticalSpace = new_vertical_space
