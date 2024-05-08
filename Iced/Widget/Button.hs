{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Button (
  button,
  onPress,
  onPressIf,
  background,
  border,
  textColor,
  hovered,
  pressed,
  disabled,
  AppearanceAttribute,
) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Attribute.Padding
import Iced.Attribute.Style
import Iced.Color
import Iced.ColorFFI
import Iced.Element

data NativeButton
type Self = Ptr NativeButton
type AttributeFn = Self -> IO Self
data NativeAppearance
type Appearance = Ptr NativeAppearance
data NativeStyleSheet
type StyleSheet = Ptr NativeStyleSheet

data Background = BgColor Color -- | BgGradient Gradient

data Border = Border {
  color :: Color,
  width :: Float,
  radius :: Float
}

--data Shadow = Shadow {
--  color :: Color,
--  offset :: (Float, Float),
--  blurRadius :: Float
--}

data AppearanceAttribute
 = ShadowOffset Float Float
 | AddBackground Background
 | TextColor Color
 | AddBorder Border
 -- | AddShadow Shadow
 | Hovered  [AppearanceAttribute]
 | Pressed  [AppearanceAttribute]
 | Disabled [AppearanceAttribute]

data Attribute message
  = AddPadding Padding
  | OnPress message
  | Style [AppearanceAttribute]
  | Width Length
  | Height Length
  | None

foreign import ccall "button_new"
  button_new :: CString -> IO Self

foreign import ccall "button_on_press"
  button_on_press :: Self -> StablePtr a -> IO Self

-- button top right bottom left
foreign import ccall "button_padding"
  button_padding :: Self -> CFloat -> CFloat -> CFloat -> CFloat -> IO Self

foreign import ccall "button_style"
  button_style :: Self -> StyleSheet -> IO Self

foreign import ccall "button_width"
  button_width :: Self -> LengthPtr -> IO Self

foreign import ccall "button_height"
  button_height :: Self -> LengthPtr -> IO Self

foreign import ccall "button_into_element"
  into_element :: Self -> IO ElementPtr

foreign import ccall "button_appearance_new"
  appearance_new :: IO Appearance

foreign import ccall "button_appearance_clone"
  appearance_clone :: Appearance -> IO Appearance

foreign import ccall "button_appearance_free"
  appearance_free :: Appearance -> IO ()

-- appearance color
foreign import ccall "button_appearance_set_background"
  set_background :: Appearance -> ColorPtr -> IO ()

-- appearance color width radius
foreign import ccall "button_appearance_set_border"
  set_border :: Appearance -> ColorPtr -> CFloat -> CFloat -> IO ()

-- appearance color
foreign import ccall "button_appearance_set_text_color"
  set_text_color :: Appearance -> ColorPtr -> IO ()

-- active
foreign import ccall "button_stylesheet_new"
  stylesheet_new :: Appearance -> IO StyleSheet

-- stylesheet appearance
foreign import ccall "button_stylesheet_set_hovered"
  stylesheet_set_hovered :: StyleSheet -> Appearance -> IO ()

-- stylesheet appearance
foreign import ccall "button_stylesheet_set_pressed"
  stylesheet_set_pressed :: StyleSheet -> Appearance -> IO ()

-- stylesheet appearance
foreign import ccall "button_stylesheet_set_disabled"
  stylesheet_set_disabled :: StyleSheet -> Appearance -> IO ()

data Button = Button {
  label :: String
}

instance Builder Self where
  build = into_element

instance IntoNative Button Self where
  toNative details = do
    label <- newCString details.label
    button_new label

instance UseAttribute Self (Attribute message) where
  useAttribute attribute = case attribute of
    OnPress message -> useOnPress message
    AddPadding value -> usePadding value
    Style value -> useStyle value
    Width  len -> useFn button_width  len
    Height len -> useFn button_height len
    None -> pure

instance PaddingToAttribute Padding (Attribute message) where
  paddingToAttribute = AddPadding

instance UseStyle [AppearanceAttribute] (Attribute message) where
  style = Style

instance UseWidth Length (Attribute message) where
  width = Width

instance UseHeight Length (Attribute message) where
  height = Height

button :: [Attribute message] -> String -> Element
button attributes label = pack Button { .. } attributes

onPress :: message -> Attribute message
onPress = OnPress

onPressIf :: Bool -> message -> Attribute message
onPressIf True message = onPress message
onPressIf False _ = None

useOnPress :: message -> AttributeFn
useOnPress message self =
  newStablePtr message
    >>= button_on_press self

usePadding :: Padding -> AttributeFn
usePadding Padding { .. } self =
  button_padding self (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)

applyStyles :: [AppearanceAttribute] -> Appearance -> IO ()
applyStyles [] _appearance = pure ()
applyStyles (first:remaining) appearance = do
  case first of
    ShadowOffset _x _y -> pure ()
    AddBackground (BgColor color) -> do
      colorPtr <- valueToNative color
      set_background appearance colorPtr
    TextColor color -> do
      colorPtr <- valueToNative color
      set_text_color appearance colorPtr
    AddBorder Border { color, width = w, radius } -> do
      colorPtr <- valueToNative color
      set_border appearance colorPtr (CFloat w) (CFloat radius)
    -- AddShadow _shadow -> pure ()
    Hovered  _attrs -> pure ()
    Pressed  _attrs -> pure ()
    Disabled _attrs -> pure ()
  applyStyles remaining appearance

border :: Color -> Float -> Float -> AppearanceAttribute
border color w radius = AddBorder $ Border color w radius

hovered :: [AppearanceAttribute] -> AppearanceAttribute
hovered = Hovered

pressed :: [AppearanceAttribute] -> AppearanceAttribute
pressed = Pressed

disabled :: [AppearanceAttribute] -> AppearanceAttribute
disabled = Disabled

findHovered :: [AppearanceAttribute] -> Maybe [AppearanceAttribute]
findHovered [] = Nothing
findHovered (first:remaining) = case first of
  Hovered attributes -> Just attributes
  _ -> findHovered remaining

findPressed :: [AppearanceAttribute] -> Maybe [AppearanceAttribute]
findPressed [] = Nothing
findPressed (first:remaining) = case first of
  Pressed attributes -> Just attributes
  _ -> findPressed remaining

findDisabled :: [AppearanceAttribute] -> Maybe [AppearanceAttribute]
findDisabled [] = Nothing
findDisabled (first:remaining) = case first of
  Disabled attributes -> Just attributes
  _ -> findDisabled remaining

useStyle :: [AppearanceAttribute] -> AttributeFn
useStyle attributes self = do
  appearance <- appearance_new
  applyStyles attributes appearance
  tmpAppearance <- appearance_clone appearance
  stylesheet <- stylesheet_new appearance
  applyHovered  attributes tmpAppearance stylesheet
  applyPressed  attributes tmpAppearance stylesheet
  applyDisabled attributes tmpAppearance stylesheet
  appearance_free tmpAppearance
  button_style self stylesheet

applyHovered :: [AppearanceAttribute] -> Appearance -> StyleSheet -> IO ()
applyHovered allAttributes active stylesheet = do
  case findHovered allAttributes of
    Just attributes -> do
      appearance <- appearance_clone active
      applyStyles attributes appearance
      stylesheet_set_hovered stylesheet appearance
    Nothing -> pure ()

applyPressed :: [AppearanceAttribute] -> Appearance -> StyleSheet -> IO ()
applyPressed allAttributes active stylesheet = do
  case findPressed allAttributes of
    Just attributes -> do
      appearance <- appearance_clone active
      applyStyles attributes appearance
      stylesheet_set_pressed stylesheet appearance
    Nothing -> pure ()

applyDisabled :: [AppearanceAttribute] -> Appearance -> StyleSheet -> IO ()
applyDisabled allAttributes active stylesheet = do
  case findDisabled allAttributes of
    Just attributes -> do
      appearance <- appearance_clone active
      applyStyles attributes appearance
      stylesheet_set_disabled stylesheet appearance
    Nothing -> pure ()

textColor :: Color -> AppearanceAttribute
textColor = TextColor

background :: Color -> AppearanceAttribute
background = AddBackground . BgColor
