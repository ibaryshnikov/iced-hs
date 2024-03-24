module Iced.Widget.Checkbox (checkbox) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Element

foreign import ccall safe "new_checkbox"
  new_checkbox :: CString -> CBool -> FunPtr (NativeOnToggle a) -> IO (Element)

type NativeOnToggle message = CBool -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnToggle message -> IO (FunPtr (NativeOnToggle message))

wrapOnToggle :: OnToggle message -> NativeOnToggle message
wrapOnToggle onToggle c_bool = do
  newStablePtr $ onToggle $ toBool c_bool

type OnToggle message = Bool -> message

checkbox :: String -> Bool -> OnToggle message -> IO (Element)
checkbox label isChecked onToggle = do
  let checked = fromBool isChecked
  labelPtr <- newCString label
  onTogglePtr <- makeCallback $ wrapOnToggle onToggle
  new_checkbox labelPtr checked onTogglePtr
