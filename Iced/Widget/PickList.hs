{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.PickList (
  pickList,
  placeholder,
) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.Length
import Iced.Attribute.LengthFFI
import Iced.Attribute.Padding
import Iced.Element

data NativePickList
type Self = Ptr NativePickList
type AttributeFn = Self -> IO Self

data Attribute = AddPadding Padding | Placeholder String | Width Length

-- len options selected on_select
foreign import ccall safe "pick_list_new"
  pick_list_new :: CUInt -> Ptr CString -> CString -> FunPtr (NativeOnSelect a) -> IO Self

-- pick_list top right bottom left
foreign import ccall safe "pick_list_padding"
  pick_list_padding :: Self -> CFloat -> CFloat -> CFloat -> CFloat -> IO Self

foreign import ccall safe "pick_list_placeholder"
  pick_list_placeholder :: Self -> CString -> IO Self

foreign import ccall safe "pick_list_width"
  pick_list_width :: Self -> LengthPtr -> IO Self

foreign import ccall safe "pick_list_into_element"
  into_element :: Self -> IO ElementPtr

type NativeOnSelect message = CString -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnSelect message -> IO (FunPtr (NativeOnSelect message))

wrapOnSelect :: Read option => OnSelect option message -> NativeOnSelect message
wrapOnSelect callback c_string = do
  string <- peekCString c_string
  newStablePtr $ callback $ read string

type OnSelect option message = option -> message

data PickList option message where
  PickList :: (Show option, Read option) => {
    attributes :: [Attribute],
    options :: [option],
    selected :: Maybe option,
    onSelect :: OnSelect option message
  } -> PickList option message

packOptions :: Show option => [option] -> [CString] -> IO ([CString])
packOptions [] strings = pure strings
packOptions (option:remaining) strings = do
  packed <- newCString $ show option
  packOptions remaining (strings ++ [packed])

selectedToString :: Show option => Maybe option -> String
selectedToString (Just option) = show option
selectedToString Nothing = "" -- treat empty string as None in Rust

instance (Show option, Read option) => IntoNative (PickList option message) where
  toNative details = do
    strings <- packOptions details.options []
    let len = fromIntegral $ length strings
    stringsPtr <- newArray strings
    selectedPtr <- newCString $ selectedToString details.selected
    onSelectPtr <- makeCallback $ wrapOnSelect details.onSelect
    self <- pick_list_new len stringsPtr selectedPtr onSelectPtr
    free stringsPtr -- Rust will free contents, but we still need to free the array itself
    into_element =<< applyAttributes details.attributes self

instance UseAttribute Self Attribute where
  useAttribute attribute = case attribute of
    AddPadding value -> usePadding value
    Placeholder value -> usePlaceholder value
    Width len -> useWidth len

instance UsePadding Attribute where
  padding v = AddPadding $ Padding v v v v

instance PaddingToAttribute Attribute where
  paddingToAttribute value = AddPadding value

instance UseWidth Length Attribute where
  width len = Width len

pickList :: (Show option, Read option) => [Attribute]
                                       -> [option]
                                       -> Maybe option
                                       -> OnSelect option message
                                       -> Element
pickList attributes options selected onSelect = pack PickList { .. }

placeholder :: String -> Attribute
placeholder value = Placeholder value

usePadding :: Padding -> AttributeFn
usePadding Padding { .. } self = do
  pick_list_padding self (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)

usePlaceholder :: String -> AttributeFn
usePlaceholder value self = do
  placeholderPtr <- newCString value
  pick_list_placeholder self placeholderPtr

useWidth :: Length -> AttributeFn
useWidth len self = do
  let nativeLen = lengthToNative len
  pick_list_width self nativeLen
