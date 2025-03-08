{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Iced.Widget.PickList (
  pickList,
  StyleAttribute,
  Status (..),
  StatusAttribute,
) where

import Control.Monad
import Data.List
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Attribute.PaddingFFI
import Iced.Attribute.Placeholder
import Iced.Attribute.Style
import Iced.ColorFFI
import Iced.Element
import Iced.Style.Internal
import Iced.Style.Status
import Iced.Theme

data NativePickList
type Self = Ptr NativePickList
type AttributeFn = Self -> IO Self
data NativeStyle
type Style = Ptr NativeStyle

data Background = BgColor Color

-- \| BgGradient Gradient

data Border = Border
  { color :: Color
  , width :: Float
  , radius :: Float
  }

data StyleAttribute
  = Background Background
  | BorderStyle Border
  | Text Color
  | PlaceholderColor Color
  | Handle Color

data Status = Active | Hovered | Opened deriving (Enum, Eq)

type StatusAttribute = (Status, [StyleAttribute])

data Attribute
  = AddPadding Padding
  | Placeholder String
  | CustomStyle StyleCallback
  | Width Length

-- len options selected on_select
foreign import ccall "pick_list_new"
  pick_list_new :: CUInt -> Ptr CString -> CString -> FunPtr (NativeOnSelect a) -> IO Self

-- pick_list padding
foreign import ccall "pick_list_padding"
  pick_list_padding :: Self -> PaddingPtr -> IO Self

foreign import ccall "pick_list_placeholder"
  pick_list_placeholder :: Self -> CString -> IO Self

foreign import ccall "pick_list_style_custom"
  pick_list_style_custom :: Self -> FunPtr NativeStyleCallback -> IO Self

foreign import ccall "pick_list_width"
  pick_list_width :: Self -> LengthPtr -> IO Self

foreign import ccall "pick_list_into_element"
  into_element :: Self -> IO ElementPtr

foreign import ccall "pick_list_style_set_background"
  set_background :: Style -> ColorPtr -> IO ()

-- style color width radius
foreign import ccall "pick_list_style_set_border"
  set_border :: Style -> ColorPtr -> CFloat -> CFloat -> IO ()

foreign import ccall "pick_list_style_set_text_color"
  set_text_color :: Style -> ColorPtr -> IO ()

foreign import ccall "pick_list_style_set_placeholder_color"
  set_placeholder_color :: Style -> ColorPtr -> IO ()

foreign import ccall "pick_list_style_set_handle_color"
  set_handle_color :: Style -> ColorPtr -> IO ()

type NativeOnSelect message = CString -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnSelect message -> IO (FunPtr (NativeOnSelect message))

wrapOnSelect :: Read option => OnSelect option message -> NativeOnSelect message
wrapOnSelect callback = newStablePtr . callback . read <=< peekCString

type OnSelect option message = option -> message

data PickList option message where
  PickList
    :: (Read option, Show option)
    => { options :: [option]
       , selected :: Maybe option
       , onSelect :: OnSelect option message
       }
    -> PickList option message

packOptions :: Show option => [option] -> [CString] -> IO ([CString])
packOptions [] strings = pure strings
packOptions (option : remaining) strings = do
  packed <- newCString $ show option
  packOptions remaining (strings ++ [packed])

selectedToString :: Show option => Maybe option -> String
selectedToString (Just option) = show option
selectedToString Nothing = "" -- treat empty string as None in Rust

instance Builder Self where
  build = into_element

instance (Read option, Show option) => IntoNative (PickList option message) Self where
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
    CustomStyle value -> useCustomStyle value
    Width len -> useFnIO pick_list_width len

instance UsePadding Attribute where
  padding = AddPadding . paddingFromOne

instance UsePadding2 Attribute where
  padding2 a b = AddPadding $ paddingFromTwo a b

instance UsePadding4 Attribute where
  padding4 top right bottom left = AddPadding Padding{..}

instance UsePlaceholder Attribute where
  placeholder = Placeholder

instance IntoStyle value => UseStyle value Attribute where
  style = intoStyle

instance UseWidth Length Attribute where
  width = Width

pickList
  :: (Read option, Show option)
  => [Attribute]
  -> [option]
  -> Maybe option
  -> OnSelect option message
  -> Element
pickList attributes options selected onSelect = pack PickList{..} attributes

usePlaceholder :: String -> AttributeFn
usePlaceholder value self = pick_list_placeholder self =<< newCString value

-- style theme status
type NativeStyleCallback = Style -> CUChar -> CUChar -> IO ()

foreign import ccall "wrapper"
  makeStyleCallback :: NativeStyleCallback -> IO (FunPtr NativeStyleCallback)

wrapStyleCallback :: StyleCallback -> NativeStyleCallback
wrapStyleCallback callback appearance themeRaw statusRaw = do
  let theme = toEnum $ fromIntegral themeRaw
  let status = toEnum $ fromIntegral statusRaw
  let attributes = callback theme status
  applyStyles attributes appearance

type StyleCallback = Theme -> Status -> [StyleAttribute]

class IntoStyle value where
  intoStyle :: value -> Attribute

instance IntoStyle [StyleAttribute] where
  intoStyle attributes = CustomStyle (\_theme _status -> attributes)

selectStyles :: [StatusAttribute] -> Status -> [StyleAttribute]
selectStyles extra status = case find (\(s, _a) -> s == status) extra of
  Just (_s, attributes) -> attributes
  Nothing -> []

instance IntoStyle [StatusAttribute] where
  intoStyle extra = CustomStyle (\_theme -> selectStyles extra)

instance IntoStyle ([StyleAttribute], [StatusAttribute]) where
  intoStyle (base, extra) = CustomStyle (\_theme -> (base ++) . selectStyles extra)

instance IntoStyle (Status -> [StyleAttribute]) where
  intoStyle callback = CustomStyle (\_theme -> callback)

instance IntoStyle StyleCallback where
  intoStyle = CustomStyle

instance UseStyleAttribute Style StyleAttribute where
  useStyleAttribute attribute = case attribute of
    Background (BgColor color) -> useFnIO set_background color
    BorderStyle value -> useBorder value
    Text color -> useFnIO set_text_color color
    PlaceholderColor color -> useFnIO set_placeholder_color color
    Handle color -> useFnIO set_handle_color color

useBorder :: Border -> Style -> IO ()
useBorder Border{color, width = w, radius} appearance = do
  colorPtr <- valueToNativeIO color
  set_border appearance colorPtr (CFloat w) (CFloat radius)

useCustomStyle :: StyleCallback -> AttributeFn
useCustomStyle callback self =
  pick_list_style_custom self
    =<< makeStyleCallback (wrapStyleCallback callback)

instance UseBackground StyleAttribute where
  background = Background . BgColor

instance UseBorder StyleAttribute where
  border color w radius = BorderStyle $ Border color w radius

instance UseTextColor StyleAttribute where
  textColor = Text

instance UsePlaceholderColor StyleAttribute where
  placeholderColor = PlaceholderColor

instance UseHandleColor StyleAttribute where
  handleColor = Handle

instance UseActive [StyleAttribute] StatusAttribute where
  active = (Active,)

instance UseHovered [StyleAttribute] StatusAttribute where
  hovered = (Hovered,)

instance UseOpened [StyleAttribute] StatusAttribute where
  opened = (Opened,)
