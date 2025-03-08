{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Iced.Widget.MouseArea (
  mouseArea,
  onDoubleClick,
  onEnter,
  onExit,
  onMiddlePress,
  onMiddleRelease,
  onMove,
  onPress,
  onRelease,
  onRightPress,
  onRightRelease,
) where

import Foreign
import Foreign.C.Types

import Iced.Attribute.OnPress
import Iced.Attribute.OnRelease
import Iced.Element

data NativeMouseArea
type Self = Ptr NativeMouseArea
type AttributeFn = Self -> IO Self
type ListenerFn message = Self -> StablePtr message -> IO Self

data Attribute message
  = OnDoubleClick message
  | OnEnter message
  | OnExit message
  | OnMiddlePress message
  | OnMiddleRelease message
  | AddOnMove (OnMove message)
  | OnPress message
  | OnRelease message
  | OnRightPress message
  | OnRightRelease message

foreign import ccall "mouse_area_new"
  mouse_area_new :: ElementPtr -> IO Self

foreign import ccall "mouse_area_on_double_click"
  on_double_click :: ListenerFn message

foreign import ccall "mouse_area_on_enter"
  on_enter :: ListenerFn message

foreign import ccall "mouse_area_on_exit"
  on_exit :: ListenerFn message

foreign import ccall "mouse_area_on_middle_press"
  on_middle_press :: ListenerFn message

foreign import ccall "mouse_area_on_middle_release"
  on_middle_release :: ListenerFn message

foreign import ccall "mouse_area_on_move"
  on_move :: Self -> FunPtr (NativeOnMove a) -> IO Self

foreign import ccall "mouse_area_on_press"
  on_press :: ListenerFn message

foreign import ccall "mouse_area_on_release"
  on_release :: ListenerFn message

foreign import ccall "mouse_area_on_right_press"
  on_right_press :: ListenerFn message

foreign import ccall "mouse_area_on_right_release"
  on_right_release :: ListenerFn message

foreign import ccall "mouse_area_into_element"
  into_element :: Self -> IO ElementPtr

type NativeOnMove message = CFloat -> CFloat -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnMove message -> IO (FunPtr (NativeOnMove message))

wrapOnMove :: OnMove message -> NativeOnMove message
wrapOnMove callback (CFloat x) (CFloat y) = newStablePtr $ callback x y

type OnMove message = Float -> Float -> message

data MouseArea = MouseArea {content :: Element}

instance Builder Self where
  build = into_element

instance IntoNative MouseArea Self where
  toNative details =
    mouse_area_new =<< elementToNative details.content

instance UseAttribute Self (Attribute message) where
  useAttribute attribute = case attribute of
    OnDoubleClick message -> useListener on_double_click message
    OnEnter message -> useListener on_enter message
    OnExit message -> useListener on_exit message
    OnMiddlePress message -> useListener on_middle_press message
    OnMiddleRelease message -> useListener on_middle_release message
    AddOnMove callback -> useOnMove callback
    OnPress message -> useListener on_press message
    OnRelease message -> useListener on_release message
    OnRightPress message -> useListener on_right_press message
    OnRightRelease message -> useListener on_right_release message

instance UseOnPress message (Attribute message) where
  onPress = OnPress

instance UseOnRelease message (Attribute message) where
  onRelease = OnRelease

mouseArea :: [Attribute message] -> Element -> Element
mouseArea attributes content = pack MouseArea{..} attributes

onDoubleClick :: message -> Attribute message
onDoubleClick = OnDoubleClick

onEnter :: message -> Attribute message
onEnter = OnEnter

onExit :: message -> Attribute message
onExit = OnExit

onMiddlePress :: message -> Attribute message
onMiddlePress = OnMiddlePress

onMiddleRelease :: message -> Attribute message
onMiddleRelease = OnMiddleRelease

onMove :: OnMove message -> Attribute message
onMove = AddOnMove

onRightPress :: message -> Attribute message
onRightPress = OnRightPress

onRightRelease :: message -> Attribute message
onRightRelease = OnRightRelease

useListener :: ListenerFn message -> message -> AttributeFn
useListener listener message self = listener self =<< newStablePtr message

useOnMove :: OnMove message -> AttributeFn
useOnMove callback self = on_move self =<< makeCallback (wrapOnMove callback)
