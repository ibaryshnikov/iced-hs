{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Widget

data Model = Model { position :: Position }

data Message = ChangePosition

update :: Model -> Message -> Model
update model message = case message of
  ChangePosition -> model { position = position } where
    position = case model.position of
      Top -> Bottom
      Bottom -> LeftSide
      LeftSide -> RightSide
      RightSide -> FollowCursor
      FollowCursor -> Top

view :: Model -> Element
view model = container [width Fill, height Fill, centerX, centerY] $
  tooltip [gap 10] content (positionToText model.position) model.position
  where content = button [onPress ChangePosition] "Press to change position"

positionToText :: Position -> Element
positionToText position = text [] label where
  label = case position of
    FollowCursor -> "Follow Cursor"
    Top -> "Top"
    Bottom -> "Bottom"
    LeftSide -> "Left"
    RightSide -> "Right"

initModel :: Model
initModel = Model { position = FollowCursor }

main :: IO ()
main = do Iced.run [] "Tooltip - Iced" initModel update view
