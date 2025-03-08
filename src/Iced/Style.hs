module Iced.Style (
  module Iced.Style.Internal,
  module Iced.Style.Status,
) where

--
-- Reexport style attributes
--

import Iced.Style.Internal (
  background,
  bar,
  border,
  borderColor,
  borderWidth,
  dotColor,
  handleColor,
  iconColor,
  placeholderColor,
  selectionColor,
  textColor,
 )
import Iced.Style.Status (
  active,
  disabled,
  focused,
  hovered,
  opened,
  pressed,
 )
