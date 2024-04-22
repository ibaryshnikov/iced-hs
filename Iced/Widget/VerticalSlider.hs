{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.VerticalSlider (
  verticalSlider,
) where

import Foreign
import Foreign.C.Types

import Iced.Attribute.Length
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
foreign import ccall safe "vertical_slider_new"
  vertical_slider_new :: CInt
                      -> CInt
                      -> CInt
                      -> FunPtr (NativeOnChange a)
                      -> IO Self

foreign import ccall safe "vertical_slider_default"
  vertical_slider_default :: Self -> CInt -> IO Self

foreign import ccall safe "vertical_slider_on_release"
  vertical_slider_on_release :: Self -> StablePtr a -> IO Self

foreign import ccall safe "vertical_slider_step"
  vertical_slider_step :: Self -> CInt -> IO Self

foreign import ccall safe "vertical_slider_shift_step"
  vertical_slider_shift_step :: Self -> CInt -> IO Self

foreign import ccall safe "vertical_slider_width"
  vertical_slider_width :: Self -> CFloat -> IO Self

foreign import ccall safe "vertical_slider_height"
  vertical_slider_height :: Self -> LengthPtr -> IO Self

foreign import ccall safe "vertical_slider_into_element"
  into_element :: Self -> IO ElementPtr


type NativeOnChange message = CInt -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnChange message -> IO (FunPtr (NativeOnChange message))

wrapOnChange :: OnChange message -> NativeOnChange message
wrapOnChange callback c_int = do
  newStablePtr $ callback $ fromIntegral c_int

type OnChange message = Int -> message

data VerticalSlider message = VerticalSlider {
  attributes :: [Attribute message],
  rangeFrom :: Int,
  rangeTo :: Int,
  value :: Int,
  onChange :: OnChange message
}

instance IntoNative (VerticalSlider message) where
  toNative details = do
    let rangeFrom = fromIntegral details.rangeFrom
    let rangeTo = fromIntegral details.rangeTo
    let value = fromIntegral details.value
    onChange <- makeCallback $ wrapOnChange details.onChange
    vertical_slider_new rangeFrom rangeTo value onChange
      >>= applyAttributes details.attributes
      >>= into_element

instance UseAttribute Self (Attribute message) where
  useAttribute attribute = do
    case attribute of
      AddDefault value -> useDefault value
      OnRelease message -> useOnRelease message
      AddStep value -> useStep value
      AddShiftStep value -> useShiftStep value
      Width len -> useWidth len
      Height value -> useHeight value

instance SliderCommon (Attribute message) where
  addDefault value = AddDefault value
  step value = AddStep value
  shiftStep value = AddShiftStep value

instance SliderCommonOnRelease message (Attribute message) where
  onRelease message = OnRelease message

instance UseWidth Float (Attribute message) where
  width value = Width value

instance UseHeight Length (Attribute message) where
  height len = Height len

verticalSlider :: [Attribute message]
               -> Int
               -> Int
               -> Int
               -> OnChange message
               -> Element
verticalSlider attributes rangeFrom rangeTo value onChange =
  pack VerticalSlider { .. }

useDefault :: Int -> AttributeFn
useDefault value self =
  vertical_slider_default self $ fromIntegral value

useOnRelease :: message -> AttributeFn
useOnRelease message self = do
  messagePtr <- newStablePtr message
  vertical_slider_on_release self messagePtr

useStep :: Int -> AttributeFn
useStep value self =
  vertical_slider_step self $ fromIntegral value

useShiftStep :: Int -> AttributeFn
useShiftStep value self =
  vertical_slider_shift_step self $ fromIntegral value

useWidth :: Float -> AttributeFn
useWidth value self = do
  vertical_slider_width self (CFloat value)

useHeight :: Length -> AttributeFn
useHeight len self = do
  let nativeLen = lengthToNative len
  vertical_slider_height self nativeLen
