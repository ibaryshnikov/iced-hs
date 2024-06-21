{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.PickList (
  pickList,
) where

import Control.Monad
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Attribute.Padding
import Iced.Attribute.PaddingFFI
import Iced.Attribute.Shared
import Iced.Element

data NativePickList
type Self = Ptr NativePickList
type AttributeFn = Self -> IO Self

data Attribute = AddPadding Padding | Placeholder String | Width Length

-- len options selected on_select
foreign import ccall "pick_list_new"
  pick_list_new :: CUInt -> Ptr CString -> CString -> FunPtr (NativeOnSelect a) -> IO Self

-- pick_list padding
foreign import ccall "pick_list_padding"
  pick_list_padding :: Self -> PaddingPtr -> IO Self

foreign import ccall "pick_list_placeholder"
  pick_list_placeholder :: Self -> CString -> IO Self

foreign import ccall "pick_list_width"
  pick_list_width :: Self -> LengthPtr -> IO Self

foreign import ccall "pick_list_into_element"
  into_element :: Self -> IO ElementPtr

type NativeOnSelect message = CString -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnSelect message -> IO (FunPtr (NativeOnSelect message))

wrapOnSelect :: Read option => OnSelect option message -> NativeOnSelect message
wrapOnSelect callback = newStablePtr . callback . read <=< peekCString

type OnSelect option message = option -> message

data PickList option message where
  PickList :: (Show option, Read option) => {
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

instance Builder Self where
  build = into_element

instance (Show option, Read option) => IntoNative (PickList option message) Self where
  toNative details = do
    strings <- packOptions details.options []
    let len = fromIntegral $ length strings
    stringsPtr <- newArray strings
    selectedPtr <- newCString $ selectedToString details.selected
    onSelectPtr <- makeCallback $ wrapOnSelect details.onSelect
    self <- pick_list_new len stringsPtr selectedPtr onSelectPtr
    free stringsPtr -- Rust will free contents, but we still need to free the array itself
    pure self

instance UseAttribute Self Attribute where
  useAttribute attribute = case attribute of
    AddPadding value -> useFnIO pick_list_padding value
    Placeholder value -> usePlaceholder value
    Width len -> useFnIO pick_list_width len

instance UsePadding Attribute where
  padding = AddPadding . paddingFromOne

instance UsePadding2 Attribute where
  padding2 a b = AddPadding $ paddingFromTwo a b

instance UsePadding4 Attribute where
  padding4 top right bottom left = AddPadding Padding { .. }

instance UsePlaceholder String Attribute where
  placeholder = Placeholder

instance UseWidth Length Attribute where
  width = Width

pickList :: (Show option, Read option) => [Attribute]
                                       -> [option]
                                       -> Maybe option
                                       -> OnSelect option message
                                       -> Element
pickList attributes options selected onSelect = pack PickList { .. } attributes

usePlaceholder :: String -> AttributeFn
usePlaceholder value self =
  newCString value
    >>= pick_list_placeholder self
