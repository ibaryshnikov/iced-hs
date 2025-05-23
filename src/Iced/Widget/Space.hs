{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

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
foreign import ccall "space_new"
  space_new :: LengthPtr -> LengthPtr -> IO Self

-- width
foreign import ccall "space_with_width"
  space_with_width :: LengthPtr -> IO Self

-- height
foreign import ccall "space_with_height"
  space_with_height :: LengthPtr -> IO Self

foreign import ccall "horizontal_space_new"
  horizontal_space_new :: IO Self

foreign import ccall "vertical_space_new"
  vertical_space_new :: IO Self

foreign import ccall "space_into_element"
  into_element :: Self -> IO ElementPtr

data Space = Space Length Length | Width Length | Height Length | Horizontal | Vertical

instance IntoNative Space ElementPtr where
  toNative details = do
    -- currently no attributes for Space
    makeSpace details
      >>= into_element

makeSpace :: Space -> IO Self
makeSpace kind = case kind of
  Space w h -> do
    widthPtr <- valueToNativeIO w
    heightPtr <- valueToNativeIO h
    space_new widthPtr heightPtr
  Width value -> space_with_width =<< valueToNativeIO value
  Height value -> space_with_height =<< valueToNativeIO value
  Horizontal -> horizontal_space_new
  Vertical -> vertical_space_new

-- todo: find a way to fix Ambiguous type variable error
-- in
-- spaceWidth :: IntoLength a => a -> Element
-- spaceWidth value = pack $ Width (intoLength value)
--
-- class IntoLength a where
--  intoLength :: a -> Length
--
-- instance IntoLength Float where
--  intoLength a = Fixed a
--
-- instance IntoLength Length where
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
