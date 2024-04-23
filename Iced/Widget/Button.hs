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
  useAttribute attribute = case attribute of
    OnPress message -> useOnPress message
    AddPadding value -> usePadding value
    Width len -> useWidth len
    Height len -> useHeight len

instance UsePadding (Attribute message) where
  padding v = AddPadding $ Padding v v v v

instance PaddingToAttribute (Attribute message) where
  paddingToAttribute = AddPadding

instance UseWidth Length (Attribute message) where
  width = Width

instance UseHeight Length (Attribute message) where
  height = Height

button :: [Attribute message] -> String -> Element
button attributes label = pack Button { .. }

onPress :: message -> Attribute message
onPress = OnPress

useOnPress :: message -> AttributeFn
useOnPress message self =
  newStablePtr message
    >>= button_on_press self

usePadding :: Padding -> AttributeFn
usePadding Padding { .. } self =
  button_padding self (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)

useWidth :: Length -> AttributeFn
useWidth len self = button_width self $ lengthToNative len

useHeight :: Length -> AttributeFn
useHeight len self = button_height self $ lengthToNative len
