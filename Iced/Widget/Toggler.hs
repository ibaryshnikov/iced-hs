{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Toggler (
  toggler,
) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Attribute.Size
import Iced.Attribute.Spacing
import Iced.Element

data NativeToggler
type Self = Ptr NativeToggler

data Attribute = Size Float | Spacing Float | Width Length

-- label is_toggled on_toggle
foreign import ccall "toggler_new"
  toggler_new :: CString -> CBool -> FunPtr (NativeOnToggle a) -> IO Self

foreign import ccall "toggler_size"
  toggler_size :: Self -> CFloat -> IO Self

foreign import ccall "toggler_spacing"
  toggler_spacing :: Self -> CFloat -> IO Self

foreign import ccall "toggler_width"
  toggler_width :: Self -> LengthPtr -> IO Self

foreign import ccall "toggler_into_element"
  into_element :: Self -> IO ElementPtr

type NativeOnToggle message = CBool -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnToggle message -> IO (FunPtr (NativeOnToggle message))

wrapOnToggle :: OnToggle message -> NativeOnToggle message
wrapOnToggle callback = newStablePtr . callback . toBool

type OnToggle message = Bool -> message

data Toggler message = Toggler {
  label :: String,
  isToggled :: Bool,
  onToggle :: OnToggle message
}

instance Builder Self where
  build = into_element

instance IntoNative (Toggler message) Self where
  toNative details = do
    label <- newCString details.label
    let isToggled = fromBool details.isToggled
    onToggle <- makeCallback $ wrapOnToggle details.onToggle
    toggler_new label isToggled onToggle

instance UseAttribute Self Attribute where
  useAttribute attribute = case attribute of
    Size value -> useFn toggler_size value
    Spacing value -> useFn toggler_spacing value
    Width len -> useFn toggler_width len

instance UseSize Attribute where
  size = Size

instance UseSpacing Attribute where
  spacing = Spacing

instance UseWidth Length Attribute where
  width = Width

toggler :: [Attribute] -> String -> Bool -> OnToggle message -> Element
toggler attributes label isToggled onToggle = pack Toggler { .. } attributes
