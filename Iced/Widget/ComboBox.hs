{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.ComboBox (
  comboBox,
  newComboBoxState,
  freeComboBoxState,
  ComboBoxState,
  onOptionHovered,
  onClose,
) where

import Control.Monad
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.LengthFFI
import Iced.Attribute.LineHeightFFI
import Iced.Attribute.OnInput
import Iced.Attribute.Padding
import Iced.Attribute.Size
import Iced.Element

data NativeComboBox
type Self = Ptr NativeComboBox
data NativeState
type State = Ptr NativeState
type ComboBoxState = State -- to export

data Attribute option message
  = AddLineHeight LineHeight
  | AddOnInput (OnInput message)
  | AddOnHover (OnOptionHovered option message)
  | AddPadding Padding
  | OnClose message
  | Size Float
  | Width Length

-- len options
foreign import ccall safe "combo_box_state_new"
  combo_box_state_new :: CUInt -> Ptr CString -> IO State

foreign import ccall safe "combo_box_state_free"
  combo_box_state_free :: State -> IO ()

-- state placeholder selected on_select
foreign import ccall safe "combo_box_new"
  combo_box_new :: State -> CString -> CString -> FunPtr (NativeOnSelect a) -> IO Self

foreign import ccall safe "combo_box_on_input"
  combo_box_on_input :: Self -> FunPtr (NativeOnInput a) -> IO Self

foreign import ccall safe "combo_box_on_option_hovered"
  combo_box_on_option_hovered :: Self -> FunPtr (NativeOnHover a) -> IO Self

foreign import ccall safe "combo_box_line_height"
  combo_box_line_height :: Self -> LineHeightPtr -> IO Self

foreign import ccall safe "combo_box_on_close"
  combo_box_on_close :: Self -> StablePtr a -> IO Self

-- combo_box top right bottom left
foreign import ccall safe "combo_box_padding"
  combo_box_padding :: Self -> CFloat -> CFloat -> CFloat -> CFloat -> IO Self

foreign import ccall safe "combo_box_size"
  combo_box_size :: Self -> CFloat -> IO Self

foreign import ccall safe "combo_box_width"
  combo_box_width :: Self -> LengthPtr -> IO Self

foreign import ccall safe "combo_box_into_element"
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
    attributes :: [Attribute option message],
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

instance (Show option, Read option) => IntoNative (ComboBox option message) where
  toNative details = do
    placeholder <- newCString details.placeholder
    selected <- newCString $ selectedToString details.selected
    onSelect <- makeOnSelectCallback $ wrapOnSelect details.onSelect
    combo_box_new details.state placeholder selected onSelect
      >>= applyAttributes details.attributes
      >>= into_element

instance Read option => UseAttribute Self (Attribute option message) where
  useAttribute attribute self = do
    case attribute of
      AddLineHeight value -> useLineHeight self value
      AddOnInput callback -> useOnInput self callback
      AddOnHover callback -> useOnHover self callback
      AddPadding value -> usePadding self value
      OnClose message -> useOnClose self message
      Size value -> useSize self value
      Width value -> useWidth self value

instance UseLineHeight (Attribute option message) where
  lineHeight = AddLineHeight

instance UseOnInput (OnInput message) (Attribute option message) where
  onInput = AddOnInput

instance UsePadding (Attribute option message) where
  padding v = AddPadding $ Padding v v v v

instance PaddingToAttribute (Attribute option message) where
  paddingToAttribute = AddPadding

instance UseSize (Attribute option message) where
  size = Size

instance UseWidth Length (Attribute option message) where
  width = Width

comboBox :: (Show option, Read option) => [Attribute option message]
                                       -> State
                                       -> String
                                       -> Maybe option
                                       -> OnSelect option message
                                       -> Element
comboBox attributes state placeholder selected onSelect = pack ComboBox { .. }

useLineHeight :: Self -> LineHeight -> IO Self
useLineHeight self = combo_box_line_height self . lineHeightToNative

onClose :: message -> Attribute option message
onClose = OnClose

useOnClose :: Self -> message -> IO Self
useOnClose self = combo_box_on_close self <=< newStablePtr

useOnInput :: Self -> OnInput message -> IO Self
useOnInput self callback = do
  onInputPtr <- makeCallback $ wrapOnInput callback
  combo_box_on_input self onInputPtr

onOptionHovered :: OnOptionHovered option message -> Attribute option message
onOptionHovered = AddOnHover

useOnHover :: Read option => Self -> OnOptionHovered option message -> IO Self
useOnHover self callback = do
  onHoverPtr <- makeOnHoverCallback $ wrapOnHover callback
  combo_box_on_option_hovered self onHoverPtr

usePadding :: Self -> Padding -> IO Self
usePadding self Padding { .. } =
  combo_box_padding self (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)

useSize :: Self -> Float -> IO Self
useSize self = combo_box_size self . CFloat

useWidth :: Self -> Length -> IO Self
useWidth self = combo_box_width self . lengthToNative

newComboBoxState :: Show option => [option] -> IO State
newComboBoxState options = do
  strings <- packOptions options []
  let len = fromIntegral $ length strings
  stringsPtr <- newArray strings
  state <- combo_box_state_new len stringsPtr
  free stringsPtr -- Rust will free contents, but we still need to free the array itself
  return state

freeComboBoxState :: State -> IO ()
freeComboBoxState = combo_box_state_free
