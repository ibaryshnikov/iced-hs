{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Slider (slider, onRelease) where

import Foreign
import Foreign.C.Types

import Iced.Attribute.Length
import Iced.Attribute.LengthFFI
import Iced.Element

data NativeSlider
type SelfPtr = Ptr NativeSlider
type AttributeFn = SelfPtr -> IO SelfPtr

data Attribute message = OnRelease message | Width Length | Height Float

-- range_from range_to value on_change
foreign import ccall safe "slider_new"
  slider_new :: CInt -> CInt -> CInt -> FunPtr (NativeOnChange a) -> IO (SelfPtr)

foreign import ccall safe "slider_on_release"
  slider_on_release :: SelfPtr -> StablePtr a -> IO (SelfPtr)

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
      OnRelease message -> useOnRelease message selfPtr
      Width len -> useWidth len selfPtr
      Height value -> useHeight value selfPtr

instance UseWidth (Attribute message) where
  width len = Width len

--instance UseHeight (Attribute message) where
--  height value = Height value

slider :: [Attribute message] -> Int -> Int -> Int -> OnChange message -> Element
slider attributes rangeFrom rangeTo value onChange = pack Slider { .. }

onRelease :: message -> Attribute message
onRelease message = OnRelease message

useOnRelease :: message -> AttributeFn
useOnRelease message selfPtr = do
  messagePtr <- newStablePtr message
  slider_on_release selfPtr messagePtr

useWidth :: Length -> AttributeFn
useWidth len selfPtr = do
  let nativeLen = lengthToNative len
  slider_width selfPtr nativeLen

useHeight :: Float -> AttributeFn
useHeight value selfPtr = do
  slider_height selfPtr (CFloat value)
