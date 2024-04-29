{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Button (
  button,
  onPress,
  onPressIf,
) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Attribute.Padding
import Iced.Element

data NativeButton
type Self = Ptr NativeButton
type AttributeFn = Self -> IO Self

data Attribute message
  = AddPadding Padding
  | OnPress message
  | Width Length
  | Height Length
  | None

foreign import ccall "button_new"
  button_new :: CString -> IO Self

foreign import ccall "button_on_press"
  button_on_press :: Self -> StablePtr a -> IO Self

-- button top right bottom left
foreign import ccall "button_padding"
  button_padding :: Self -> CFloat -> CFloat -> CFloat -> CFloat -> IO Self

foreign import ccall "button_width"
  button_width :: Self -> LengthPtr -> IO Self

foreign import ccall "button_height"
  button_height :: Self -> LengthPtr -> IO Self

foreign import ccall "button_into_element"
  into_element :: Self -> IO ElementPtr

data Button = Button {
  label :: String
}

instance Builder Self where
  build = into_element

instance IntoNative Button Self where
  toNative details = do
    label <- newCString details.label
    button_new label

instance UseAttribute Self (Attribute message) where
  useAttribute attribute = case attribute of
    OnPress message -> useOnPress message
    AddPadding value -> usePadding value
    Width  len -> useFn button_width  len
    Height len -> useFn button_height len
    None -> pure

instance PaddingToAttribute Padding (Attribute message) where
  paddingToAttribute = AddPadding

instance UseWidth Length (Attribute message) where
  width = Width

instance UseHeight Length (Attribute message) where
  height = Height

button :: [Attribute message] -> String -> Element
button attributes label = pack Button { .. } attributes

onPress :: message -> Attribute message
onPress = OnPress

onPressIf :: Bool -> message -> Attribute message
onPressIf True message = onPress message
onPressIf False _ = None

useOnPress :: message -> AttributeFn
useOnPress message self =
  newStablePtr message
    >>= button_on_press self

usePadding :: Padding -> AttributeFn
usePadding Padding { .. } self =
  button_padding self (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)
