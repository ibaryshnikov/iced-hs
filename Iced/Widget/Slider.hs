{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Slider (
  slider,
) where

import Foreign
import Foreign.C.Types

import Iced.Attribute.Length
import Iced.Attribute.LengthFFI
import Iced.Attribute.SliderCommon
import Iced.Element

data NativeSlider
type Self = Ptr NativeSlider
type AttributeFn = Self -> IO Self

data Attribute message
  = OnRelease message
  | AddDefault Int
  | AddStep Int
  | AddShiftStep Int
  | Width Length
  | Height Float

-- range_from range_to value on_change
foreign import ccall safe "slider_new"
  slider_new :: CInt -> CInt -> CInt -> FunPtr (NativeOnChange a) -> IO Self

foreign import ccall safe "slider_default"
  slider_default :: Self -> CInt -> IO Self

foreign import ccall safe "slider_on_release"
  slider_on_release :: Self -> StablePtr a -> IO Self

foreign import ccall safe "slider_step"
  slider_step :: Self -> CInt -> IO Self

foreign import ccall safe "slider_shift_step"
  slider_shift_step :: Self -> CInt -> IO Self

foreign import ccall safe "slider_width"
  slider_width :: Self -> LengthPtr -> IO Self

foreign import ccall safe "slider_height"
  slider_height :: Self -> CFloat -> IO Self

foreign import ccall safe "slider_into_element"
  into_element :: Self -> IO ElementPtr


type NativeOnChange message = CInt -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnChange message -> IO (FunPtr (NativeOnChange message))

wrapOnChange :: OnChange message -> NativeOnChange message
wrapOnChange callback c_int = do
  newStablePtr $ callback $ fromIntegral c_int

type OnChange message = Int -> message

data Slider message = Slider {
  attributes :: [Attribute message],
  rangeFrom :: Int,
  rangeTo :: Int,
  value :: Int,
  onChange :: OnChange message
}

instance IntoNative (Slider message) where
  toNative details = do
    let rangeFrom = fromIntegral details.rangeFrom
    let rangeTo = fromIntegral details.rangeTo
    let value = fromIntegral details.value
    onChangePtr <- makeCallback $ wrapOnChange details.onChange
    self <- slider_new rangeFrom rangeTo value onChangePtr
    into_element =<< applyAttributes self details.attributes

instance UseAttribute Self (Attribute message) where
  useAttribute self attribute = do
    case attribute of
      AddDefault value -> useDefault value self
      OnRelease message -> useOnRelease message self
      AddStep value -> useStep value self
      AddShiftStep value -> useShiftStep value self
      Width len -> useWidth len self
      Height value -> useHeight value self

instance SliderCommon (Attribute message) where
  addDefault value = AddDefault value
  step value = AddStep value
  shiftStep value = AddShiftStep value

instance SliderCommonOnRelease message (Attribute message) where
  onRelease message = OnRelease message

instance UseWidth Length (Attribute message) where
  width len = Width len

instance UseHeight Float (Attribute message) where
  height value = Height value

slider :: [Attribute message] -> Int -> Int -> Int -> OnChange message -> Element
slider attributes rangeFrom rangeTo value onChange = pack Slider { .. }

useDefault :: Int -> AttributeFn
useDefault value self = slider_default self $ fromIntegral value

useOnRelease :: message -> AttributeFn
useOnRelease message self = do
  messagePtr <- newStablePtr message
  slider_on_release self messagePtr

useStep :: Int -> AttributeFn
useStep value self = slider_step self $ fromIntegral value

useShiftStep :: Int -> AttributeFn
useShiftStep value self = slider_shift_step self $ fromIntegral value

useWidth :: Length -> AttributeFn
useWidth len self = do
  let nativeLen = lengthToNative len
  slider_width self nativeLen

useHeight :: Float -> AttributeFn
useHeight value self = slider_height self (CFloat value)
