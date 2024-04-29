{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Radio (
  radio,
) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Element

data NativeRadio
type Self = Ptr NativeRadio

data Attribute = Width Length

-- label value selected on_select
foreign import ccall "radio_new"
  radio_new :: CString -> CUInt -> CUInt -> FunPtr (NativeOnClick a) -> IO Self

foreign import ccall "radio_width"
  radio_width :: Self -> LengthPtr -> IO Self

foreign import ccall "radio_into_element"
  into_element :: Self -> IO ElementPtr

type NativeOnClick message = CUInt -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnClick message -> IO (FunPtr (NativeOnClick message))

wrapOnClick :: Enum option => OnClick option message -> NativeOnClick message
wrapOnClick callback =
  newStablePtr . callback . optionFromInt . fromIntegral

type OnClick option message = option -> message

data Radio option message where
  Radio :: Enum option => {
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

instance Builder Self where
  build = into_element

instance Enum option => IntoNative (Radio option message) Self where
  toNative details = do
    label <- newCString details.label
    let value = fromIntegral $ optionToInt details.value
    let selected = fromIntegral $ selectedToInt details.selected
    onSelect <- makeCallback $ wrapOnClick details.onClick
    radio_new label value selected onSelect

instance UseAttribute Self Attribute where
  useAttribute attribute = case attribute of
      Width len -> useFn radio_width len

instance UseWidth Length Attribute where
  width = Width

radio :: Enum option
      => [Attribute]
      -> String
      -> option
      -> Maybe option
      -> OnClick option message
      -> Element
radio attributes label value selected onClick = pack Radio { .. } attributes
