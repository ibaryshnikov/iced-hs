{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Iced.Widget.TextEditor (
  textEditor,
  newContent,
  contentWithText,
  perform,
  Action,
  Content,
  onAction,
  StyleAttribute,
  Status (..),
  StatusAttribute,
) where

import Control.Monad
import Data.List (find)
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Attribute.PaddingFFI
import Iced.Attribute.Style
import Iced.ColorFFI
import Iced.Element
import Iced.Style.Internal
import Iced.Style.Status
import Iced.Theme

data NativeTextEditor
type Self = Ptr NativeTextEditor
type AttributeFn = Self -> IO Self
data NativeContent
type Content = ForeignPtr NativeContent
data NativeAction
type Action = Ptr NativeAction
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
  | Placeholder Color
  | Text Color
  | Selection Color

data Status = Active | Hovered | Focused | Disabled deriving (Enum, Eq)

type StatusAttribute = (Status, [StyleAttribute])

data Attribute message
  = AddOnAction (OnAction message)
  | AddPadding Padding
  | CustomStyle StyleCallback
  | Height Length

foreign import ccall "text_editor_content_new"
  content_new :: IO (Ptr NativeContent)

makeContent :: Ptr NativeContent -> IO Content
makeContent = newForeignPtr content_free

newContent :: IO Content
newContent = makeContent =<< content_new

foreign import ccall "text_editor_content_with_text"
  content_with_text :: CString -> IO (Ptr NativeContent)

contentWithText :: String -> IO Content
contentWithText = makeContent <=< content_with_text <=< newCString

foreign import ccall "text_editor_content_perform"
  content_perform :: Ptr NativeContent -> Action -> IO ()

perform :: Content -> Action -> IO ()
perform content action = withForeignPtr content $ \self ->
  content_perform self action

foreign import ccall "&text_editor_content_free"
  content_free :: FinalizerPtr NativeContent

foreign import ccall "text_editor_new"
  text_editor_new :: Ptr NativeContent -> IO Self

foreign import ccall "text_editor_on_action"
  text_editor_on_action :: Self -> FunPtr (NativeOnAction message) -> IO Self

-- text_editor padding
foreign import ccall "text_editor_padding"
  text_editor_padding :: Self -> PaddingPtr -> IO Self

foreign import ccall "text_editor_style_custom"
  text_editor_style_custom :: Self -> FunPtr NativeStyleCallback -> IO Self

foreign import ccall "text_editor_height"
  text_editor_height :: Self -> LengthPtr -> IO Self

foreign import ccall "text_editor_into_element"
  into_element :: Self -> IO ElementPtr

foreign import ccall "text_editor_style_set_background"
  set_background :: Style -> ColorPtr -> IO ()

-- style color width radius
foreign import ccall "text_editor_style_set_border"
  set_border :: Style -> ColorPtr -> CFloat -> CFloat -> IO ()

foreign import ccall "text_editor_style_set_placeholder"
  set_placeholder :: Style -> ColorPtr -> IO ()

foreign import ccall "text_editor_style_set_value"
  set_value :: Style -> ColorPtr -> IO ()

foreign import ccall "text_editor_style_set_selection"
  set_selection :: Style -> ColorPtr -> IO ()

type NativeOnAction message = Action -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnAction message -> IO (FunPtr (NativeOnAction message))

wrapOnAction :: OnAction message -> NativeOnAction message
wrapOnAction callback = newStablePtr . callback

type OnAction message = Action -> message

data TextEditor message = TextEditor
  { content :: Content
  }

instance Builder Self where
  build = into_element

instance IntoNative (TextEditor message) Self where
  toNative details = do
    withForeignPtr details.content text_editor_new

instance UseAttribute Self (Attribute message) where
  useAttribute attribute = case attribute of
    AddOnAction callback -> useOnAction callback
    AddPadding value -> useFnIO text_editor_padding value
    CustomStyle value -> useCustomStyle value
    Height len -> useFnIO text_editor_height len

instance UsePadding (Attribute message) where
  padding = AddPadding . paddingFromOne

instance UsePadding2 (Attribute message) where
  padding2 a b = AddPadding $ paddingFromTwo a b

instance UsePadding4 (Attribute message) where
  padding4 top right bottom left = AddPadding Padding{..}

instance IntoStyle value => UseStyle value (Attribute message) where
  style = intoStyle

instance UseHeight Length (Attribute message) where
  height = Height

textEditor :: [Attribute message] -> Content -> Element
textEditor attributes content = pack TextEditor{..} attributes

onAction :: OnAction message -> Attribute message
onAction = AddOnAction

useOnAction :: OnAction message -> AttributeFn
useOnAction callback self =
  makeCallback (wrapOnAction callback)
    >>= text_editor_on_action self

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
  intoStyle :: value -> Attribute message

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
    Placeholder color -> useFnIO set_placeholder color
    Text color -> useFnIO set_value color
    Selection color -> useFnIO set_selection color

useBorder :: Border -> Style -> IO ()
useBorder Border{color, width = w, radius} appearance = do
  colorPtr <- valueToNativeIO color
  set_border appearance colorPtr (CFloat w) (CFloat radius)

useCustomStyle :: StyleCallback -> AttributeFn
useCustomStyle callback self =
  text_editor_style_custom self
    =<< makeStyleCallback (wrapStyleCallback callback)

instance UseBackground StyleAttribute where
  background = Background . BgColor

instance UseBorder StyleAttribute where
  border color w radius = BorderStyle $ Border color w radius

instance UsePlaceholderColor StyleAttribute where
  placeholderColor = Placeholder

instance UseTextColor StyleAttribute where
  textColor = Text

instance UseSelectionColor StyleAttribute where
  selectionColor = Selection

instance UseActive [StyleAttribute] StatusAttribute where
  active = (Active,)

instance UseHovered [StyleAttribute] StatusAttribute where
  hovered = (Hovered,)

instance UseFocused [StyleAttribute] StatusAttribute where
  focused = (Focused,)

instance UseDisabled [StyleAttribute] StatusAttribute where
  disabled = (Disabled,)
