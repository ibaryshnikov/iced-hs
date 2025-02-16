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
import Iced.Theme
import Iced.Time
import Iced.Widget
import Iced.Widget.Canvas qualified as Canvas
import Iced.Widget.Canvas.Shape
import Iced.Widget.Canvas.FrameAction

data Screen = Start | Game | Win

data Moving = Moving { left :: Bool, right :: Bool }

data Position = Position { x :: Float, y :: Float }
data Bullet = Bullet { position :: Position }
data Enemy = Enemy { position :: Position }

initialEnemies :: Integer -> [Enemy]
initialEnemies 0 = []
initialEnemies n = map newEnemy [1..n]

newEnemy :: Integer -> Enemy
newEnemy n = Enemy { position = Position { .. } }
  where x = (fromIntegral n) * 50 + 25
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

update :: Message -> Model -> IO Model
update StartGame = pure . startGame
update (Pressed code) = pure . handleKey code True
update (Released code) = pure . handleKey code False
update (Tick micros) = handleTick micros

startGame :: Model -> Model
startGame model = case model.screen of
  Game -> model
  Start -> newGame model
  Win   -> newGame model

newGame :: Model -> Model
newGame model = model {
    screen = Game,
    enemies = initialEnemies 10,
    position = Position { x = 320, y = 400 }
  }

handleKey :: KeyCode -> Bool -> Model -> Model
handleKey code isPressed model = case code of
  KeyA -> model { moving = model.moving { left  = isPressed } }
  KeyD -> model { moving = model.moving { right = isPressed } }
  Space -> model { shooting = isPressed }
  _ -> model

handleTick :: Integer -> Model -> IO Model
handleTick micros oldModel = do
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
  where position = Position { x = ship.x - 2, y = ship.y }

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

updateBulletPosition :: Bullet -> Float -> Bullet
updateBulletPosition bullet distance = Bullet { position = position }
  where
    position = bullet.position { y = bullet.position.y - distance }

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

updateScreen :: Screen -> [Enemy] -> Screen
updateScreen Game [] = Win
updateScreen screen _enemies = screen

view :: Model -> Element
view model = center [] $
  case model.screen of
    Start -> button [onPress StartGame] "Start"
    Game -> canvas [width (Fixed 640), height (Fixed 480)] (shapes model) model.state
    Win -> column [alignX Alignment.Center, spacing 30] [
        text [size 22] "You won!",
        button [onPress StartGame] "Start new game"
      ]

fillRectangle :: Float -> Float -> Float -> Float -> Color -> FrameAction
fillRectangle x y width_ height_ color_ = fill [
    rectangle x y width_ height_
  ] color_

shapes :: Model -> [FrameAction]
shapes model = [drawField, drawShip model.position]
  ++ map drawBullet model.bullets
  ++ map drawEnemy model.enemies

drawField :: FrameAction
drawField = fillRectangle 0 0 640 480 (rgb8 0 0 0)

drawShip :: Position -> FrameAction
drawShip Position { .. } = fillRectangle (x - 50) y 100 30 (rgb8 180 180 180)

drawBullet :: Bullet -> FrameAction
drawBullet bullet = fillRectangle x y 4 20 $ rgb8 180 180 180
  where Position { .. } = bullet.position

drawEnemy :: Enemy -> FrameAction
drawEnemy enemy = fillRectangle x y 40 40 $ rgb8 180 180 180
  where Position { .. } = enemy.position

subscriptionFn :: Model -> IO (Subscription Message)
subscriptionFn model = case model.screen of
  Start -> Subscription.none
  Win -> Subscription.none
  Game -> Subscription.batch [
      onKeyPress Pressed,
      onKeyRelease Released,
      everyMillis 15 Tick
    ]

main :: IO ()
main = do
  model <- newModel
  Iced.run [subscription subscriptionFn, theme Oxocarbon]
    "Space shooter" model update view
