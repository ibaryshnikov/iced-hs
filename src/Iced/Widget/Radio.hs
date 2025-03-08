{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Iced.Widget.Radio (
  radio,
  StyleAttribute,
  Status (..),
  StatusAttribute,
) where

import Data.List
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Attribute.Style
import Iced.ColorFFI
import Iced.Element
import Iced.Style.Internal
import Iced.Style.Status
import Iced.Theme

data NativeRadio
type Self = Ptr NativeRadio
type AttributeFn = Self -> IO Self
data NativeStyle
type Style = Ptr NativeStyle

data Background = BgColor Color

-- \| BgGradient Gradient

data StyleAttribute
  = Background Background
  | DotColor Color
  | BorderWidth Float
  | BorderColor Color
  | TextColor Color

data Status = Active | Hovered deriving (Enum, Eq)

type StatusAttribute = (Status, [StyleAttribute])

data Attribute
  = CustomStyle StyleCallback
  | Width Length

-- label value selected on_select
foreign import ccall "radio_new"
  radio_new :: CString -> CUInt -> CUInt -> FunPtr (NativeOnClick a) -> IO Self

foreign import ccall "radio_style_custom"
  radio_style_custom :: Self -> FunPtr NativeStyleCallback -> IO Self

foreign import ccall "radio_width"
  radio_width :: Self -> LengthPtr -> IO Self

foreign import ccall "radio_into_element"
  into_element :: Self -> IO ElementPtr

foreign import ccall "radio_style_set_background"
  set_background :: Style -> ColorPtr -> IO ()

foreign import ccall "radio_style_set_dot_color"
  set_dot_color :: Style -> ColorPtr -> IO ()

foreign import ccall "radio_style_set_border_width"
  set_border_width :: Style -> CFloat -> IO ()

foreign import ccall "radio_style_set_border_color"
  set_border_color :: Style -> ColorPtr -> IO ()

foreign import ccall "radio_style_set_text_color"
  set_text_color :: Style -> ColorPtr -> IO ()

type NativeOnClick message = CUInt -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnClick message -> IO (FunPtr (NativeOnClick message))

wrapOnClick :: Enum option => OnClick option message -> NativeOnClick message
wrapOnClick callback = newStablePtr . callback . optionFromInt . fromIntegral

type OnClick option message = option -> message

data Radio option message where
  Radio
    :: Enum option
    => { label :: String
       , value :: option
       , selected :: Maybe option
       , onClick :: OnClick option message
       }
    -> Radio option message

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
    CustomStyle value -> useCustomStyle value
    Width len -> useFnIO radio_width len

instance IntoStyle value => UseStyle value Attribute where
  style = intoStyle

instance UseWidth Length Attribute where
  width = Width

radio
  :: Enum option
  => [Attribute]
  -> String
  -> option
  -> Maybe option
  -> OnClick option message
  -> Element
radio attributes label value selected onClick = pack Radio{..} attributes

-- style theme status is_selected
type NativeStyleCallback = Style -> CUChar -> CUChar -> CBool -> IO ()

foreign import ccall "wrapper"
  makeStyleCallback :: NativeStyleCallback -> IO (FunPtr NativeStyleCallback)

wrapStyleCallback :: StyleCallback -> NativeStyleCallback
wrapStyleCallback callback appearance themeRaw statusRaw isSelectedRaw = do
  let theme = toEnum $ fromIntegral themeRaw
  let status = toEnum $ fromIntegral statusRaw
  let isSelected = toBool isSelectedRaw
  let attributes = callback theme status isSelected
  applyStyles attributes appearance

type StyleCallback = Theme -> Status -> Bool -> [StyleAttribute]

class IntoStyle value where
  intoStyle :: value -> Attribute

instance IntoStyle [StyleAttribute] where
  intoStyle attributes = CustomStyle (\_theme _status _isSelected -> attributes)

selectStyles :: [StatusAttribute] -> Status -> [StyleAttribute]
selectStyles extra status = case find (\(s, _a) -> s == status) extra of
  Just (_s, attributes) -> attributes
  Nothing -> []

instance IntoStyle [StatusAttribute] where
  intoStyle extra = CustomStyle (\_theme status _isSelected -> selectStyles extra status)

instance IntoStyle ([StyleAttribute], [StatusAttribute]) where
  intoStyle (base, extra) = CustomStyle (\_theme status _isSelected -> base ++ selectStyles extra status)

instance IntoStyle (Status -> [StyleAttribute]) where
  intoStyle callback = CustomStyle (\_theme status _isSelected -> callback status)

instance IntoStyle (Bool -> [StyleAttribute]) where
  intoStyle callback = CustomStyle (\_theme _status -> callback)

instance IntoStyle (Status -> Bool -> [StyleAttribute]) where
  intoStyle callback = CustomStyle (\_theme -> callback)

instance IntoStyle StyleCallback where
  intoStyle callback = CustomStyle callback

instance UseStyleAttribute Style StyleAttribute where
  useStyleAttribute attribute = case attribute of
    Background (BgColor color) -> useFnIO set_background color
    DotColor color -> useFnIO set_dot_color color
    BorderWidth value -> useFn set_border_width value
    BorderColor color -> useFnIO set_border_color color
    TextColor color -> useFnIO set_text_color color

useCustomStyle :: StyleCallback -> AttributeFn
useCustomStyle callback self =
  radio_style_custom self
    =<< makeStyleCallback (wrapStyleCallback callback)

instance UseBackground StyleAttribute where
  background = Background . BgColor

instance UseDotColor StyleAttribute where
  dotColor = DotColor

instance UseBorderWidth StyleAttribute where
  borderWidth = BorderWidth

instance UseBorderColor StyleAttribute where
  borderColor = BorderColor

instance UseTextColor StyleAttribute where
  textColor = TextColor

instance UseActive [StyleAttribute] StatusAttribute where
  active = (Active,)

instance UseHovered [StyleAttribute] StatusAttribute where
  hovered = (Hovered,)
