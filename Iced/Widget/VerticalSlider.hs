{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.VerticalSlider (
  verticalSlider,
) where

import Foreign
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Attribute.SliderCommon
import Iced.Element

data NativeVerticalSlider
type Self = Ptr NativeVerticalSlider
type AttributeFn = Self -> IO Self

data Attribute message
  = OnRelease message
  | AddDefault Int
  | AddStep Int
  | AddShiftStep Int
  | Width Float
  | Height Length

-- range_from range_to value on_change
foreign import ccall "vertical_slider_new"
  vertical_slider_new :: CInt
                      -> CInt
                      -> CInt
                      -> FunPtr (NativeOnChange a)
                      -> IO Self

foreign import ccall "vertical_slider_default"
  vertical_slider_default :: Self -> CInt -> IO Self

foreign import ccall "vertical_slider_on_release"
  vertical_slider_on_release :: Self -> StablePtr a -> IO Self

foreign import ccall "vertical_slider_step"
  vertical_slider_step :: Self -> CInt -> IO Self

foreign import ccall "vertical_slider_shift_step"
  vertical_slider_shift_step :: Self -> CInt -> IO Self

foreign import ccall "vertical_slider_width"
  vertical_slider_width :: Self -> CFloat -> IO Self

foreign import ccall "vertical_slider_height"
  vertical_slider_height :: Self -> LengthPtr -> IO Self

foreign import ccall "vertical_slider_into_element"
  into_element :: Self -> IO ElementPtr

type NativeOnChange message = CInt -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnChange message -> IO (FunPtr (NativeOnChange message))

wrapOnChange :: OnChange message -> NativeOnChange message
wrapOnChange callback = newStablePtr . callback . fromIntegral

type OnChange message = Int -> message

data VerticalSlider message = VerticalSlider {
  rangeFrom :: Int,
  rangeTo :: Int,
  value :: Int,
  onChange :: OnChange message
}

instance Builder Self where
  build = into_element

instance IntoNative (VerticalSlider message) Self where
  toNative details = do
    let rangeFrom = fromIntegral details.rangeFrom
    let rangeTo = fromIntegral details.rangeTo
    let value = fromIntegral details.value
    onChange <- makeCallback $ wrapOnChange details.onChange
    vertical_slider_new rangeFrom rangeTo value onChange

instance UseAttribute Self (Attribute message) where
  useAttribute attribute = case attribute of
    AddDefault value -> useFn vertical_slider_default value
    OnRelease message -> useOnRelease message
    AddStep value -> useFn vertical_slider_step value
    AddShiftStep value -> useFn vertical_slider_shift_step value
    Width  len -> useFn vertical_slider_width  len
    Height len -> useFnIO vertical_slider_height len

instance SliderCommon (Attribute message) where
  addDefault = AddDefault
  step = AddStep
  shiftStep = AddShiftStep

instance SliderCommonOnRelease message (Attribute message) where
  onRelease = OnRelease

instance UseWidth Float (Attribute message) where
  width = Width

instance UseHeight Length (Attribute message) where
  height = Height

verticalSlider :: [Attribute message]
               -> Int
               -> Int
               -> Int
               -> OnChange message
               -> Element
verticalSlider attributes rangeFrom rangeTo value onChange =
  pack VerticalSlider { .. } attributes

useOnRelease :: message -> AttributeFn
useOnRelease message self =
  newStablePtr message
    >>= vertical_slider_on_release self
