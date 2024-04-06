{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Space (
  space,
  spaceWidth,
  spaceHeight,
  horizontalSpace,
  verticalSpace,
) where

import Foreign

import Iced.Element
import Iced.Length
import Iced.LengthFFI

data NativeSpace
type SelfPtr = Ptr NativeSpace

-- width height
foreign import ccall safe "space_new"
  space_new :: LengthPtr -> LengthPtr -> IO (SelfPtr)

-- width
foreign import ccall safe "space_with_width"
  space_with_width :: LengthPtr -> IO (SelfPtr)

-- height
foreign import ccall safe "space_with_height"
  space_with_height :: LengthPtr -> IO (SelfPtr)

foreign import ccall safe "horizontal_space_new"
  horizontal_space_new :: IO (SelfPtr)

foreign import ccall safe "vertical_space_new"
  vertical_space_new :: IO (SelfPtr)

foreign import ccall safe "space_into_element"
  space_into_element :: SelfPtr -> IO (ElementPtr)

data Space = Space Length Length | Width Length | Height Length | Horizontal | Vertical

instance IntoNative Space where
  toNative details = do
    selfPtr <- makeSpace details
    -- currently no attributes for Space
    -- updatedSelf <- applyAttributes selfPtr details.attributes
    space_into_element selfPtr

--instance UseAttribute SelfPtr Attribute where
--  useAttribute selfPtr attribute = do
--    case attribute of
--      Width len -> useWidth len selfPtr
--      Height len -> useHeight len selfPtr

--instance UseLength Attribute where
--  width len = Width len
--  height len = Height len

makeSpace :: Space -> IO (SelfPtr)
makeSpace kind = case kind of
  Space w h -> space_new widthPtr heightPtr where
    widthPtr = lengthToNative w
    heightPtr = lengthToNative h
  Width value -> space_with_width $ lengthToNative value
  Height value -> space_with_height $ lengthToNative value
  Horizontal -> horizontal_space_new
  Vertical -> vertical_space_new

space :: Length -> Length -> Element
space w h = pack $ Space w h

spaceWidth :: Length -> Element
spaceWidth value = pack $ Width value

spaceHeight :: Length -> Element
spaceHeight value = pack $ Height value

horizontalSpace :: Element
horizontalSpace = pack Horizontal

verticalSpace :: Element
verticalSpace = pack Vertical
