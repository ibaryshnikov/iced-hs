{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.ComboBox (
  comboBox,
  newState,
  State,
  onOptionHovered,
  onClose,
) where

import Control.Monad
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Attribute.LineHeightFFI
import Iced.Attribute.OnInput
import Iced.Attribute.Padding
import Iced.Attribute.Size
import Iced.Element

data NativeComboBox
type Self = Ptr NativeComboBox
data NativeState
type State = ForeignPtr NativeState
type AttributeFn = Self -> IO Self

data Attribute option message
  = AddLineHeight LineHeight
  | AddOnInput (OnInput message)
  | AddOnHover (OnOptionHovered option message)
  | AddPadding Padding
  | OnClose message
  | Size Float
  | Width Length

-- len options
foreign import ccall "combo_box_state_new"
  state_new :: CUInt -> Ptr CString -> IO (Ptr NativeState)

newState :: Show option => [option] -> IO State
newState options = do
  strings <- packOptions options []
  let len = fromIntegral $ length strings
  stringsPtr <- newArray strings
  state <- newForeignPtr state_free =<< state_new len stringsPtr
  free stringsPtr -- Rust will free contents, but we still need to free the array itself
  return state

foreign import ccall "&combo_box_state_free"
  state_free :: FinalizerPtr NativeState

-- state placeholder selected on_select
foreign import ccall "combo_box_new"
  combo_box_new :: Ptr NativeState -> CString -> CString -> FunPtr (NativeOnSelect a) -> IO Self

foreign import ccall "combo_box_on_input"
  combo_box_on_input :: Self -> FunPtr (NativeOnInput a) -> IO Self

foreign import ccall "combo_box_on_option_hovered"
  combo_box_on_option_hovered :: Self -> FunPtr (NativeOnHover a) -> IO Self

foreign import ccall "combo_box_line_height"
  combo_box_line_height :: Self -> LineHeightPtr -> IO Self

foreign import ccall "combo_box_on_close"
  combo_box_on_close :: Self -> StablePtr a -> IO Self

-- combo_box top right bottom left
foreign import ccall "combo_box_padding"
  combo_box_padding :: Self -> CFloat -> CFloat -> CFloat -> CFloat -> IO Self

foreign import ccall "combo_box_size"
  combo_box_size :: Self -> CFloat -> IO Self

foreign import ccall "combo_box_width"
  combo_box_width :: Self -> LengthPtr -> IO Self

foreign import ccall "combo_box_into_element"
  into_element :: Self -> IO ElementPtr

type NativeOnSelect message = CString -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeOnSelectCallback :: NativeOnSelect message -> IO (FunPtr (NativeOnSelect message))

wrapOnSelect :: Read option => OnSelect option message -> NativeOnSelect message
wrapOnSelect callback = newStablePtr . callback . read <=< peekCString

type NativeOnInput message = CString -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnInput message -> IO (FunPtr (NativeOnInput message))

wrapOnInput :: OnInput message -> NativeOnInput message
wrapOnInput callback = newStablePtr . callback <=< peekCString

type NativeOnHover message = CString -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeOnHoverCallback :: NativeOnHover message -> IO (FunPtr (NativeOnHover message))

wrapOnHover :: Read option => OnOptionHovered option message -> NativeOnHover message
wrapOnHover callback = newStablePtr . callback . read <=< peekCString

type OnSelect option message = option -> message
type OnInput message = String -> message
type OnOptionHovered option message = option -> message

data ComboBox option message where
  ComboBox :: (Show option, Read option) => {
    state :: State,
    placeholder :: String,
    selected :: Maybe option,
    onSelect :: OnSelect option message
  } -> ComboBox option message

packOptions :: Show option => [option] -> [CString] -> IO ([CString])
packOptions [] strings = pure strings
packOptions (option:remaining) strings = do
  packed <- newCString $ show option
  packOptions remaining (strings ++ [packed])

selectedToString :: Show option => Maybe option -> String
selectedToString (Just option) = show option
selectedToString Nothing = "" -- treat empty string as None in Rust

instance Builder Self where
  build = into_element

instance (Show option, Read option) => IntoNative (ComboBox option message) Self where
  toNative details = do
    placeholder <- newCString details.placeholder
    selected <- newCString $ selectedToString details.selected
    onSelect <- makeOnSelectCallback $ wrapOnSelect details.onSelect
    withForeignPtr details.state $ \state ->
      combo_box_new state placeholder selected onSelect

instance Read option => UseAttribute Self (Attribute option message) where
  useAttribute attribute = case attribute of
    AddLineHeight value -> useFnIO combo_box_line_height value
    AddOnInput callback -> useOnInput callback
    AddOnHover callback -> useOnHover callback
    AddPadding value -> usePadding value
    OnClose message -> useOnClose message
    Size  value -> useFn combo_box_size  value
    Width value -> useFnIO combo_box_width value

instance UseLineHeight (Attribute option message) where
  lineHeight = AddLineHeight

instance UseOnInput (OnInput message) (Attribute option message) where
  onInput = AddOnInput

instance PaddingToAttribute Padding (Attribute option message) where
  paddingToAttribute = AddPadding

instance UseSize (Attribute option message) where
  size = Size

instance UseWidth Length (Attribute option message) where
  width = Width

comboBox :: (Show option, Read option)
         => [Attribute option message]
         -> State
         -> String
         -> Maybe option
         -> OnSelect option message
         -> Element
comboBox attributes state placeholder selected onSelect = pack ComboBox { .. } attributes

onClose :: message -> Attribute option message
onClose = OnClose

useOnClose :: message -> AttributeFn
useOnClose message self =
  newStablePtr message
    >>= combo_box_on_close self

useOnInput :: OnInput message -> AttributeFn
useOnInput callback self =
  makeCallback (wrapOnInput callback)
    >>= combo_box_on_input self

onOptionHovered :: OnOptionHovered option message -> Attribute option message
onOptionHovered = AddOnHover

useOnHover :: Read option => OnOptionHovered option message -> AttributeFn
useOnHover callback self =
  makeOnHoverCallback (wrapOnHover callback)
    >>= combo_box_on_option_hovered self

usePadding :: Padding -> AttributeFn
usePadding Padding { .. } self =
  combo_box_padding self (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)
