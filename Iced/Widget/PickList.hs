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

import Iced.Element
import Iced.Length
import Iced.LengthFFI

data NativePickList
type SelfPtr = Ptr NativePickList
type AttributeFn = SelfPtr -> IO SelfPtr

data Attribute = Placeholder String | Width Length

-- len options selected on_select
foreign import ccall safe "pick_list_new"
  pick_list_new :: CUInt -> Ptr CString -> CString -> FunPtr (NativeOnSelect a) -> IO (SelfPtr)

foreign import ccall safe "pick_list_placeholder"
  pick_list_placeholder :: SelfPtr -> CString -> IO (SelfPtr)

foreign import ccall safe "pick_list_width"
  pick_list_width :: SelfPtr -> LengthPtr -> IO (SelfPtr)

foreign import ccall safe "pick_list_into_element"
  pick_list_into_element :: SelfPtr -> IO (ElementPtr)

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
    selfPtr <- pick_list_new len stringsPtr selectedPtr onSelectPtr
    free stringsPtr -- Rust will free contents, but we still need to free the array itself
    updatedSelf <- applyAttributes selfPtr details.attributes
    pick_list_into_element updatedSelf

instance UseAttribute SelfPtr Attribute where
  useAttribute selfPtr attribute = do
    case attribute of
      Placeholder value -> usePlaceholder value selfPtr
      Width len -> useWidth len selfPtr

instance UseWidth Attribute where
  width len = Width len

pickList :: (Show option, Read option) => [Attribute]
                                       -> [option]
                                       -> Maybe option
                                       -> OnSelect option message
                                       -> Element
pickList attributes options selected onSelect = pack PickList { .. }

placeholder :: String -> Attribute
placeholder value = Placeholder value

usePlaceholder :: String -> AttributeFn
usePlaceholder value selfPtr = do
  placeholderPtr <- newCString value
  pick_list_placeholder selfPtr placeholderPtr

useWidth :: Length -> AttributeFn
useWidth len selfPtr = do
  let nativeLen = lengthToNative len
  pick_list_width selfPtr nativeLen