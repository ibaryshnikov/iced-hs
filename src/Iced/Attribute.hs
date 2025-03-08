module Iced.Attribute (
  -- shared attributes
  module Iced.Attribute.Alignment,
  module Iced.Attribute.Color,
  module Iced.Attribute.Icon,
  module Iced.Attribute.Length,
  module Iced.Attribute.LineHeight,
  module Iced.Attribute.OnInput,
  module Iced.Attribute.OnPress,
  module Iced.Attribute.OnRelease,
  module Iced.Attribute.Padding,
  module Iced.Attribute.Placeholder,
  module Iced.Attribute.Size,
  module Iced.Attribute.SliderCommon,
  module Iced.Attribute.Spacing,
  module Iced.Attribute.Style,
  -- widget attributes
  module Iced.Widget.Button,
  module Iced.Widget.Checkbox,
  module Iced.Widget.ComboBox,
  module Iced.Widget.Container,
  module Iced.Widget.MouseArea,
  module Iced.Widget.TextEditor,
  module Iced.Widget.TextInput,
  module Iced.Widget.Tooltip,
) where

--
-- Reexport shared attributes
--

import Iced.Attribute.Alignment (alignX, alignY)
import Iced.Attribute.Color (color)
import Iced.Attribute.Icon (icon)
import Iced.Attribute.Length (Length (..), height, width)
import Iced.Attribute.LineHeight (LineHeight (..), lineHeight)
import Iced.Attribute.OnInput (onInput)
import Iced.Attribute.OnPress (onPress)
import Iced.Attribute.OnRelease (onRelease)
import Iced.Attribute.Padding (padding, padding2, padding4)
import Iced.Attribute.Placeholder (placeholder)
import Iced.Attribute.Size (size)
import Iced.Attribute.SliderCommon (addDefault, shiftStep, step)
import Iced.Attribute.Spacing (spacing)
import Iced.Attribute.Style (style)
--
-- Reexport widget attributes
--

import Iced.Widget.Button (onPressIf)
import Iced.Widget.Checkbox (
  onToggle,
  onToggleIf,
  textLineHeight,
  textShaping,
  textSize,
 )
import Iced.Widget.ComboBox (
  onClose,
  onOptionHovered,
 )
import Iced.Widget.Container (
  centerX,
  centerY,
 )
import Iced.Widget.MouseArea (
  onDoubleClick,
  onEnter,
  onExit,
  onMiddlePress,
  onMiddleRelease,
  onMove,
  onRightPress,
  onRightRelease,
 )
import Iced.Widget.TextEditor (onAction)
import Iced.Widget.TextInput (onSubmit)
import Iced.Widget.Tooltip (
  gap,
  snapWithViewport,
  tooltip,
 )
