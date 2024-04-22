{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Button (button, onPress) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.Length
import Iced.Attribute.LengthFFI
import Iced.Attribute.Padding
import Iced.Element

data NativeButton
type Self = Ptr NativeButton
type AttributeFn = Self -> IO Self

data Attribute message = AddPadding Padding | OnPress message | Width Length | Height Length

foreign import ccall safe "button_new"
  button_new :: CString -> IO Self

foreign import ccall safe "button_on_press"
  button_on_press :: Self -> StablePtr a -> IO Self

-- button top right bottom left
foreign import ccall safe "button_padding"
  button_padding :: Self -> CFloat -> CFloat -> CFloat -> CFloat -> IO Self

foreign import ccall safe "button_width"
  button_width :: Self -> LengthPtr -> IO Self

foreign import ccall safe "button_height"
  button_height :: Self -> LengthPtr -> IO Self

foreign import ccall safe "button_into_element"
  into_element :: Self -> IO ElementPtr

data Button message = Button {
  attributes :: [Attribute message],
  label :: String
}

instance IntoNative (Button message) where
  toNative details = do
    label <- newCString details.label
    button_new label
      >>= applyAttributes details.attributes
      >>= into_element

instance UseAttribute Self (Attribute message) where
  useAttribute attribute = do
    case attribute of
      OnPress message -> useOnPress message
      AddPadding value -> usePadding value
      Width len -> useWidth len
      Height len -> useHeight len

instance UsePadding (Attribute message) where
  padding v = AddPadding $ Padding v v v v

instance PaddingToAttribute (Attribute message) where
  paddingToAttribute value = AddPadding value

instance UseWidth Length (Attribute message) where
  width len = Width len

instance UseHeight Length (Attribute message) where
  height len = Height len

button :: [Attribute message] -> String -> Element
button attributes label = pack Button { .. }

onPress :: message -> Attribute message
onPress message = OnPress message

useOnPress :: message -> AttributeFn
useOnPress message self = do
  messagePtr <- newStablePtr message
  button_on_press self messagePtr

usePadding :: Padding -> AttributeFn
usePadding Padding { .. } self = do
  button_padding self (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)

useWidth :: Length -> AttributeFn
useWidth len self = do
  let nativeLen = lengthToNative len
  button_width self nativeLen

useHeight :: Length -> AttributeFn
useHeight len self = do
  let nativeLen = lengthToNative len
  button_height self nativeLen
