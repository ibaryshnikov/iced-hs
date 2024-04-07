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

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Element

data NativeComboBox
type SelfPtr = Ptr NativeComboBox
type AttributeFn = SelfPtr -> IO SelfPtr
data NativeState
type State = Ptr NativeState
type ComboBoxState = State

data Attribute option message
  = AddOnHover (OnOptionHovered option message)
  | OnClose message

-- len options
foreign import ccall safe "combo_box_state_new"
  combo_box_state_new :: CUInt -> Ptr CString -> IO (State)

foreign import ccall safe "combo_box_state_free"
  combo_box_state_free :: State -> IO ()

-- state placeholder selected on_select
foreign import ccall safe "combo_box_new"
  combo_box_new :: State -> CString -> CString -> FunPtr (NativeOnSelect a) -> IO (SelfPtr)

foreign import ccall safe "combo_box_on_option_hovered"
  combo_box_on_option_hovered :: SelfPtr -> FunPtr (NativeOnHover a) -> IO (SelfPtr)

foreign import ccall safe "combo_box_on_close"
  combo_box_on_close :: SelfPtr -> StablePtr a -> IO (SelfPtr)

foreign import ccall safe "combo_box_into_element"
  combo_box_into_element :: SelfPtr -> IO (ElementPtr)

type NativeOnSelect message = CString -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeOnSelectCallback :: NativeOnSelect message -> IO (FunPtr (NativeOnSelect message))

wrapOnSelect :: Read option => OnSelect option message -> NativeOnSelect message
wrapOnSelect callback c_string = do
  string <- peekCString c_string
  newStablePtr $ callback $ read string

type NativeOnHover message = CString -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeOnHoverCallback :: NativeOnHover message -> IO (FunPtr (NativeOnHover message))

wrapOnHover :: Read option => OnSelect option message -> NativeOnHover message
wrapOnHover callback c_string = do
  string <- peekCString c_string
  newStablePtr $ callback $ read string

type OnSelect option message = option -> message
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
    placeholderPtr <- newCString details.placeholder
    selectedPtr <- newCString $ selectedToString details.selected
    onSelectPtr <- makeOnSelectCallback $ wrapOnSelect details.onSelect
    selfPtr <- combo_box_new details.state placeholderPtr selectedPtr onSelectPtr
    updatedSelf <- applyAttributes selfPtr details.attributes
    combo_box_into_element updatedSelf

instance Read option => UseAttribute SelfPtr (Attribute option message) where
  useAttribute selfPtr attribute = do
    case attribute of
      AddOnHover callback -> useOnHover callback selfPtr
      OnClose message -> useOnClose message selfPtr

comboBox :: (Show option, Read option) => [Attribute option message]
                                       -> State
                                       -> String
                                       -> Maybe option
                                       -> OnSelect option message
                                       -> Element
comboBox attributes state placeholder selected onSelect = pack ComboBox { .. }

onClose :: message -> Attribute option message
onClose message = OnClose message

useOnClose :: message -> AttributeFn
useOnClose message selfPtr = do
  -- pass a callback instead which will create
  -- a new StablePtr for each message separately
  messagePtr <- newStablePtr message
  combo_box_on_close selfPtr messagePtr

onOptionHovered :: OnOptionHovered option message -> Attribute option message
onOptionHovered callback = AddOnHover callback

useOnHover :: Read option => OnOptionHovered option message -> AttributeFn
useOnHover callback selfPtr = do
  onHoverPtr <- makeOnHoverCallback $ wrapOnHover callback
  combo_box_on_option_hovered selfPtr onHoverPtr

newComboBoxState :: Show option => [option] -> IO (State)
newComboBoxState options = do
  strings <- packOptions options []
  let len = fromIntegral $ length strings
  stringsPtr <- newArray strings
  state <- combo_box_state_new len stringsPtr
  free stringsPtr -- Rust will free contents, but we still need to free the array itself
  return state

freeComboBoxState :: State -> IO ()
freeComboBoxState = combo_box_state_free
