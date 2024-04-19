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
type SelfPtr = Ptr NativeVerticalSlider
type AttributeFn = SelfPtr -> IO SelfPtr

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
                      -> IO (SelfPtr)

foreign import ccall safe "vertical_slider_default"
  vertical_slider_default :: SelfPtr -> CInt -> IO (SelfPtr)

foreign import ccall safe "vertical_slider_on_release"
  vertical_slider_on_release :: SelfPtr -> StablePtr a -> IO (SelfPtr)

foreign import ccall safe "vertical_slider_step"
  vertical_slider_step :: SelfPtr -> CInt -> IO (SelfPtr)

foreign import ccall safe "vertical_slider_shift_step"
  vertical_slider_shift_step :: SelfPtr -> CInt -> IO (SelfPtr)

foreign import ccall safe "vertical_slider_width"
  vertical_slider_width :: SelfPtr -> CFloat -> IO (SelfPtr)

foreign import ccall safe "vertical_slider_height"
  vertical_slider_height :: SelfPtr -> LengthPtr -> IO (SelfPtr)

foreign import ccall safe "vertical_slider_into_element"
  vertical_slider_into_element :: SelfPtr -> IO (ElementPtr)


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
    onChangePtr <- makeCallback $ wrapOnChange details.onChange
    selfPtr <- vertical_slider_new rangeFrom rangeTo value onChangePtr
    updatedSelf <- applyAttributes selfPtr details.attributes
    vertical_slider_into_element updatedSelf

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
useDefault value selfPtr =
  vertical_slider_default selfPtr $ fromIntegral value

useOnRelease :: message -> AttributeFn
useOnRelease message selfPtr = do
  messagePtr <- newStablePtr message
  vertical_slider_on_release selfPtr messagePtr

useStep :: Int -> AttributeFn
useStep value selfPtr =
  vertical_slider_step selfPtr $ fromIntegral value

useShiftStep :: Int -> AttributeFn
useShiftStep value selfPtr =
  vertical_slider_shift_step selfPtr $ fromIntegral value

useWidth :: Float -> AttributeFn
useWidth value selfPtr = do
  vertical_slider_width selfPtr (CFloat value)

useHeight :: Length -> AttributeFn
useHeight len selfPtr = do
  let nativeLen = lengthToNative len
  vertical_slider_height selfPtr nativeLen
