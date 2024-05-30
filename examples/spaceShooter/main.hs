{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Attribute.Alignment qualified as Alignment
import Iced.Color
import Iced.Keyboard
import Iced.Keyboard.PhysicalKey (KeyCode(..))
import Iced.Subscription qualified as Subscription
import Iced.Time
import Iced.Widget
import Iced.Widget.Canvas qualified as Canvas
import Iced.Widget.Canvas.Shape
import Iced.Widget.Canvas.FrameAction

data Screen = Start | Game | Win

data Moving = Moving {
  left :: Bool,
  right :: Bool
}

data Position = Position {
  x :: Float,
  y :: Float
}

data Bullet = Bullet {
  position :: Position
}

data Enemy = Enemy {
  position :: Position
}

initialEnemies :: Integer -> [Enemy]
initialEnemies 0 = []
initialEnemies n = [enemy] ++ (initialEnemies $ n - 1)
  where enemy = Enemy { position = Position { .. } }
        x = (fromIntegral n) * 50 + 25
        y = 60

data Model = Model {
  state :: Canvas.State,
  moving :: Moving,
  position :: Position,
  bullets :: [Bullet],
  enemies :: [Enemy],
  shooting :: Bool,
  lastShot :: Integer,
  lastTick :: Integer,
  screen :: Screen
}

newModel :: IO Model
newModel = do
  state <- Canvas.newState
  lastTick <- microsSinceStart
  let moving = Moving { left = False, right = False }
  let position = Position { x = 320, y = 400 }
  let bullets = []
  let enemies = initialEnemies 10
  let lastShot = 0
  let shooting = False
  let screen = Start
  pure Model { .. }

data Message
  = StartGame
  | Pressed KeyCode
  | Released KeyCode
  | Tick Integer

update :: Model -> Message -> IO Model
update model StartGame = case model.screen of
  Game -> pure model
  Start -> startGame model
  Win   -> startGame model
update model (Pressed code) = pure $ case code of
  KeyA -> handleMove model $ Left True
  KeyD -> handleMove model $ Right True
  Space -> model { shooting = True }
  _ -> model
update model (Released code) = pure $ case code of
  KeyA -> handleMove model $ Left False
  KeyD -> handleMove model $ Right False
  Space -> model { shooting = False }
  _ -> model
update oldModel (Tick micros) = do
  model <- shoot oldModel
  Canvas.clearCache model.state
  let diff = (fromIntegral (micros - model.lastTick)) / 10_000
  let position = updatePosition model.moving model.position diff
  let newBullets = updateBullets model.bullets diff
  let (bullets, enemies) = checkCollisions newBullets [] model.enemies
  let screen = updateScreen model.screen enemies
  pure model {
    lastTick = micros,
    position = position,
    bullets = bullets,
    enemies = enemies,
    screen = screen
  }

startGame :: Model -> IO Model
startGame model = pure model {
    screen = Game,
    enemies = initialEnemies 10,
    position = Position { x = 320, y = 400 }
  }

handleMove :: Model -> Either Bool Bool -> Model
handleMove model direction = model { moving = moving }
  where
    moving = case direction of
      Left  isPressed -> model.moving { left  = isPressed }
      Right isPressed -> model.moving { right = isPressed }

shoot :: Model -> IO Model
shoot model = case model.shooting of
  True -> do
    now <- microsSinceStart
    let diff = now - model.lastShot
    if diff < 150_000
    then pure model
    else pure model { bullets = bullets, lastShot = now }
      where bullets = model.bullets ++ [makeBullet model.position]
  False -> pure model

makeBullet :: Position -> Bullet
makeBullet ship = Bullet { .. }
  where position = Position { x = ship.x, y = ship.y }

updatePosition :: Moving -> Position -> Float -> Position
updatePosition moving position distance = case (moving.left, moving.right) of
  (True, False) -> position { x = position.x - distance } -- moving left
  (False, True) -> position { x = position.x + distance } -- moving right
  _ -> position

updateBullets :: [Bullet] -> Float -> [Bullet]
updateBullets bullets distance = filter inMap $ map updateOne bullets
  where
    updateOne bullet = updateBulletPosition bullet (distance * 2)
    inMap bullet = bullet.position.y > 0

checkCollisions :: [Bullet] -> [Bullet] -> [Enemy] -> ([Bullet], [Enemy])
checkCollisions [] bullets enemies = (bullets, enemies)
checkCollisions (first:remaining) acc enemies =
  case checkHit first enemies [] of
    (Just _one, rest) -> checkCollisions remaining acc rest
    (Nothing, _rest)  -> checkCollisions remaining (acc ++ [first]) enemies

checkHit :: Bullet -> [Enemy] -> [Enemy] -> (Maybe Enemy, [Enemy])
checkHit _bullet [] acc = (Nothing, acc)
checkHit bullet (first:remaining) acc =
  if intersectsWith bullet first
  then (Just first, acc ++ remaining)
  else checkHit bullet remaining (acc ++ [first])

intersectsWith :: Bullet -> Enemy -> Bool
intersectsWith bullet enemy =
  intersectsInDimension a.x (a.x + 4)  b.x (b.x + 40) &&
  intersectsInDimension a.y (a.y + 20) b.y (b.y + 40)
  where
    a = bullet.position
    b = enemy.position

intersectsInDimension :: Float -> Float -> Float -> Float -> Bool
intersectsInDimension ax1 ax2 bx1 bx2 =
  pointInRange ax1 ax2 bx1 ||
  pointInRange ax1 ax2 bx2 ||
  pointInRange bx1 bx2 ax1 ||
  pointInRange bx1 bx2 ax2

pointInRange :: Float -> Float -> Float -> Bool
pointInRange from to x = x > from && x < to

updateBulletPosition :: Bullet -> Float -> Bullet
updateBulletPosition bullet distance = Bullet { position = position }
  where
    position = bullet.position { y = bullet.position.y - distance }

updateScreen :: Screen -> [Enemy] -> Screen
updateScreen Game [] = Win
updateScreen screen _enemies = screen

view :: Model -> Element
view model = container [centerX Fill, centerY Fill] $
  case model.screen of
    Start -> button [onPress StartGame] "Start"
    Game -> canvas [width (Fixed 640), height (Fixed 480)] (shapes model) model.state
    Win -> column [alignItems Alignment.Center, spacing 20] [
        text [] "You won!",
        button [onPress StartGame] "Start new game"
      ]

fillRectangle :: Float -> Float -> Float -> Float -> Color -> FrameAction
fillRectangle x y width_ height_ color = fill [
    rectangle x y width_ height_
  ] color

shapes :: Model -> [FrameAction]
shapes model = [
    fillRectangle 0 0 640 480 (rgb8 0 0 0), -- draw field
    drawShip model.position
  ]
  ++ drawBullets model.bullets
  ++ drawEnemies model.enemies

drawShip :: Position -> FrameAction
drawShip Position { .. } = fillRectangle (x - 50) y 100 30 (rgb8 255 255 255)

drawBullets :: [Bullet] -> [FrameAction]
drawBullets = map (\item -> drawOne item.position)
  where drawOne Position { .. } = fillRectangle x y 4 20 $ rgb8 255 255 255

drawEnemies :: [Enemy] -> [FrameAction]
drawEnemies = map (\item -> drawOne item.position)
  where drawOne Position { .. } = fillRectangle x y 40 40 $ rgb8 255 255 255

subscriptionFn :: Model -> IO (Subscription Message)
subscriptionFn model = case model.screen of
  Start -> Subscription.none
  Win -> Subscription.none
  Game -> Subscription.batch [
      onKeyPress Pressed,
      onKeyRelease Released,
      every (durationFromMillis 15) Tick
    ]

main :: IO ()
main = do
  model <- newModel
  Iced.run [subscription subscriptionFn] "Space shooter" model update view
