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
type SelfPtr = Ptr NativeSlider
type AttributeFn = SelfPtr -> IO SelfPtr

data Attribute message
  = OnRelease message
  | AddDefault Int
  | AddStep Int
  | AddShiftStep Int
  | Width Length
  | Height Float

-- range_from range_to value on_change
foreign import ccall safe "slider_new"
  slider_new :: CInt -> CInt -> CInt -> FunPtr (NativeOnChange a) -> IO (SelfPtr)

foreign import ccall safe "slider_default"
  slider_default :: SelfPtr -> CInt -> IO (SelfPtr)

foreign import ccall safe "slider_on_release"
  slider_on_release :: SelfPtr -> StablePtr a -> IO (SelfPtr)

foreign import ccall safe "slider_step"
  slider_step :: SelfPtr -> CInt -> IO (SelfPtr)

foreign import ccall safe "slider_shift_step"
  slider_shift_step :: SelfPtr -> CInt -> IO (SelfPtr)

foreign import ccall safe "slider_width"
  slider_width :: SelfPtr -> LengthPtr -> IO (SelfPtr)

foreign import ccall safe "slider_height"
  slider_height :: SelfPtr -> CFloat -> IO (SelfPtr)

foreign import ccall safe "slider_into_element"
  slider_into_element :: SelfPtr -> IO (ElementPtr)


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
    selfPtr <- slider_new rangeFrom rangeTo value onChangePtr
    updatedSelf <- applyAttributes selfPtr details.attributes
    slider_into_element updatedSelf

instance UseAttribute SelfPtr (Attribute message) where
  useAttribute selfPtr attribute = do
    case attribute of
      AddDefault value -> useDefault value selfPtr
      OnRelease message -> useOnRelease message selfPtr
      AddStep value -> useStep value selfPtr
      AddShiftStep value -> useShiftStep value selfPtr
      Width len -> useWidth len selfPtr
      Height value -> useHeight value selfPtr

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
useDefault value selfPtr = slider_default selfPtr $ fromIntegral value

useOnRelease :: message -> AttributeFn
useOnRelease message selfPtr = do
  messagePtr <- newStablePtr message
  slider_on_release selfPtr messagePtr

useStep :: Int -> AttributeFn
useStep value selfPtr = slider_step selfPtr $ fromIntegral value

useShiftStep :: Int -> AttributeFn
useShiftStep value selfPtr = slider_shift_step selfPtr $ fromIntegral value

useWidth :: Length -> AttributeFn
useWidth len selfPtr = do
  let nativeLen = lengthToNative len
  slider_width selfPtr nativeLen

useHeight :: Float -> AttributeFn
useHeight value selfPtr = slider_height selfPtr (CFloat value)
