{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Toggler (
  toggler,
) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.Length
import Iced.Attribute.LengthFFI
import Iced.Attribute.Size
import Iced.Attribute.Spacing
import Iced.Element

data NativeToggler
type Self = Ptr NativeToggler
type AttributeFn = Self -> IO Self

data Attribute = Size Float | Spacing Float | Width Length

-- label is_toggled on_toggle
foreign import ccall safe "toggler_new"
  toggler_new :: CString -> CBool -> FunPtr (NativeOnToggle a) -> IO Self

foreign import ccall safe "toggler_size"
  toggler_size :: Self -> CFloat -> IO Self

foreign import ccall safe "toggler_spacing"
  toggler_spacing :: Self -> CFloat -> IO Self

foreign import ccall safe "toggler_width"
  toggler_width :: Self -> LengthPtr -> IO Self

foreign import ccall safe "toggler_into_element"
  into_element :: Self -> IO ElementPtr

type NativeOnToggle message = CBool -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnToggle message -> IO (FunPtr (NativeOnToggle message))

wrapOnToggle :: OnToggle message -> NativeOnToggle message
wrapOnToggle callback c_bool = do
  newStablePtr $ callback $ toBool c_bool

type OnToggle message = Bool -> message

data Toggler message = Toggler {
  attributes :: [Attribute],
  label :: String,
  isToggled :: Bool,
  onToggle :: OnToggle message
}

instance IntoNative (Toggler message) where
  toNative details = do
    label <- newCString details.label
    let isToggled = fromBool details.isToggled
    onToggle <- makeCallback $ wrapOnToggle details.onToggle
    toggler_new label isToggled onToggle
      >>= applyAttributes details.attributes
      >>= into_element

instance UseAttribute Self Attribute where
  useAttribute attribute = do
    case attribute of
      Size value -> useSize value
      Spacing value -> useSpacing value
      Width len -> useWidth len

instance UseSize Attribute where
  size value = Size value

instance UseSpacing Attribute where
  spacing value = Spacing value

instance UseWidth Length Attribute where
  width len = Width len

toggler :: [Attribute] -> String -> Bool -> OnToggle message -> Element
toggler attributes label isToggled onToggle = pack Toggler { .. }

useSize :: Float -> AttributeFn
useSize value self = do
  toggler_size self (CFloat value)

useSpacing :: Float -> AttributeFn
useSpacing value self = do
  toggler_spacing self (CFloat value)

useWidth :: Length -> AttributeFn
useWidth len self = do
  let nativeLen = lengthToNative len
  toggler_width self nativeLen
