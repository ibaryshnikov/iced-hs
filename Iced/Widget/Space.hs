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

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Element

data NativeSpace
type Self = Ptr NativeSpace

-- width height
foreign import ccall safe "space_new"
  space_new :: LengthPtr -> LengthPtr -> IO Self

-- width
foreign import ccall safe "space_with_width"
  space_with_width :: LengthPtr -> IO Self

-- height
foreign import ccall safe "space_with_height"
  space_with_height :: LengthPtr -> IO Self

foreign import ccall safe "horizontal_space_new"
  horizontal_space_new :: IO Self

foreign import ccall safe "vertical_space_new"
  vertical_space_new :: IO Self

foreign import ccall safe "space_into_element"
  into_element :: Self -> IO ElementPtr

data Space = Space Length Length | Width Length | Height Length | Horizontal | Vertical

instance IntoNative Space ElementPtr where
  toNative details = do
    -- currently no attributes for Space
    makeSpace details
      >>= into_element

makeSpace :: Space -> IO Self
makeSpace kind = case kind of
  Space w h -> space_new widthPtr heightPtr where
    widthPtr  = valueToNative w
    heightPtr = valueToNative h
  Width  value -> space_with_width  $ valueToNative value
  Height value -> space_with_height $ valueToNative value
  Horizontal -> horizontal_space_new
  Vertical -> vertical_space_new

-- todo: find a way to fix Ambiguous type variable error
-- in
-- spaceWidth :: IntoLength a => a -> Element
-- spaceWidth value = pack $ Width (intoLength value)
--
--class IntoLength a where
--  intoLength :: a -> Length
--
--instance IntoLength Float where
--  intoLength a = Fixed a
--
--instance IntoLength Length where
--  intoLength a = a

space :: Length -> Length -> Element
space w h = packSimple $ Space w h

spaceWidth :: Length -> Element
spaceWidth value = packSimple $ Width value

spaceHeight :: Length -> Element
spaceHeight value = packSimple $ Height value

horizontalSpace :: Element
horizontalSpace = packSimple Horizontal

verticalSpace :: Element
verticalSpace = packSimple Vertical
