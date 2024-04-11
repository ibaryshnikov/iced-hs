{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Toggler (
  toggler,
) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Element
import Iced.Length
import Iced.LengthFFI
import Iced.Size
import Iced.Spacing

data NativeToggler
type SelfPtr = Ptr NativeToggler
type AttributeFn = SelfPtr -> IO SelfPtr

data Attribute = Size Float | Spacing Float | Width Length

-- label is_toggled on_toggle
foreign import ccall safe "toggler_new"
  toggler_new :: CString -> CBool -> FunPtr (NativeOnToggle a) -> IO (SelfPtr)

foreign import ccall safe "toggler_size"
  toggler_size :: SelfPtr -> CFloat -> IO (SelfPtr)

foreign import ccall safe "toggler_spacing"
  toggler_spacing :: SelfPtr -> CFloat -> IO (SelfPtr)

foreign import ccall safe "toggler_width"
  toggler_width :: SelfPtr -> LengthPtr -> IO (SelfPtr)

foreign import ccall safe "toggler_into_element"
  toggler_into_element :: SelfPtr -> IO (ElementPtr)

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
    labelPtr <- newCString details.label
    let isToggled = fromBool details.isToggled
    onTogglePtr <- makeCallback $ wrapOnToggle details.onToggle
    selfPtr <- toggler_new labelPtr isToggled onTogglePtr
    updatedSelf <- applyAttributes selfPtr details.attributes
    toggler_into_element updatedSelf

instance UseAttribute SelfPtr Attribute where
  useAttribute selfPtr attribute = do
    case attribute of
      Size value -> useSize value selfPtr
      Spacing value -> useSpacing value selfPtr
      Width len -> useWidth len selfPtr

instance UseSize Attribute where
  size value = Size value

instance UseSpacing Attribute where
  spacing value = Spacing value

instance UseWidth Attribute where
  width len = Width len

toggler :: [Attribute] -> String -> Bool -> OnToggle message -> Element
toggler attributes label isToggled onToggle = pack Toggler { .. }

useSize :: Float -> AttributeFn
useSize value selfPtr = do
  toggler_size selfPtr (CFloat value)

useSpacing :: Float -> AttributeFn
useSpacing value selfPtr = do
  toggler_spacing selfPtr (CFloat value)

useWidth :: Length -> AttributeFn
useWidth len selfPtr = do
  let nativeLen = lengthToNative len
  toggler_width selfPtr nativeLen
