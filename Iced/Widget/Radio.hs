{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Radio (
  radio,
) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Element

data NativeRadio
type SelfPtr = Ptr NativeRadio
type AttributeFn = SelfPtr -> IO SelfPtr

data Attribute

-- label value selected on_select
foreign import ccall safe "radio_new"
  radio_new :: CString -> CUInt -> CUInt -> FunPtr (NativeOnClick a) -> IO (SelfPtr)

foreign import ccall safe "radio_into_element"
  radio_into_element :: SelfPtr -> IO (ElementPtr)

type NativeOnClick message = CUInt -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnClick message -> IO (FunPtr (NativeOnClick message))

wrapOnClick :: Enum option => OnClick option message -> NativeOnClick message
wrapOnClick callback input = do
  let option = optionFromInt $ fromIntegral input
  newStablePtr $ callback option

type OnClick option message = option -> message

data Radio option message where
  Radio :: Enum option => {
    attributes :: [Attribute],
    label :: String,
    value :: option,
    selected :: Maybe option,
    onClick :: OnClick option message
  } -> Radio option message

-- convert to 1-indexed, reserve 0 for Nothing
optionToInt :: Enum option => option -> Int
optionToInt option = 1 + fromEnum option

 -- convert back to 0-indexed
optionFromInt :: Enum option => Int -> option
optionFromInt input = toEnum $ input - 1 -- will be a runtime error in case of mistake

selectedToInt :: Enum option => Maybe option -> Int
selectedToInt (Just option) = optionToInt option
selectedToInt Nothing = 0

instance Enum option => IntoNative (Radio option message) where
  toNative details = do
    labelPtr <- newCString details.label
    let value = fromIntegral $ optionToInt details.value
    let selected = fromIntegral $ selectedToInt details.selected
    onSelectPtr <- makeCallback $ wrapOnClick details.onClick
    selfPtr <- radio_new labelPtr value selected onSelectPtr
    updatedSelf <- applyAttributes selfPtr details.attributes
    radio_into_element updatedSelf

instance UseAttribute SelfPtr Attribute where
  useAttribute selfPtr _attribute = pure selfPtr

radio :: Enum option
      => [Attribute]
      -> String -> option -> Maybe option -> OnClick option message -> Element
radio attributes label value selected onClick = pack Radio { .. }
