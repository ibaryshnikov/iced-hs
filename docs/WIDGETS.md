# Widgets

List of all widgets

```haskell
import Iced.Widget
```

[button](#button) [canvas](#canvas) [checkbox](#checkbox) [column](#column)
[comboBox](#combobox) [container](#container) [image](#image) [markdown](#markdown)
[pickList](#picklist) [progressBar](#progressbar) [radio](#radio)
[responsive](#responsive) [row](#row) [scrollable](#scrollable) [slider](#slider)
[space](#space) [text](#text) [textEditor](#texteditor) [textInput](#textinput)
[toggler](#toggler) [tooltip](#tooltip)


## Button

```haskell
button :: [Attribute message] -> String -> Element

-- attributes

-- emit the message when the button is clicked
onPress :: message -> Attribute message

-- if True emit the message when the button is clicked
onPressIf :: Bool -> message -> Attribute message

-- set the padding for all sides
padding :: Float -> Attribute message

-- set the padding as following:
-- first argument is top and bottom
-- second is left and right
padding2 :: Float -> Float -> Attribute message

-- set the padding as following:
-- top right bottom left
padding4 :: Float -> Float -> Float -> Float -> Attribute message

-- set the style of the button
style :: IntoStyle value => value -> Attribute message

-- set the width of the button
width :: Length -> Attribute message

-- set the height of the button
height :: Length -> Attribute message

-- example
button [onPress Click] "Click me"
```


## Canvas

Detailed [canvas api](./CANVAS.md)

```haskell
-- canvas requires State, which is a draw cache
newState :: IO State
-- you need to clear the cache when you want a redraw
clearCache :: State -> IO ()
canvas :: [Attribute] -> [FrameAction] -> State -> Element

-- example
import Iced.Color
import Iced.Widget
import Iced.Widget.Canvas qualified as Canvas
import Iced.Widget.Canvas.Shape
import Iced.Widget.Canvas.FrameAction

data Model = Model { state :: Canvas.State }

-- make some state and put it into model
state <- Canvas.newState

-- clear cache in the update function when you want a redraw
Canvas.clearCache model.state

-- use widget in view
canvas [width Fill, height Fill] shapes model.state

shapes :: [FrameAction]
shapes = [
    fill [
      circle 700 700 50
    ] $ rgb8 150 200 50,
    stroke [
      circle 300 500 70
    ] (rgb8 255 50 50) 4
  ]
```


## Checkbox

```haskell
checkbox :: [Attribute message] -> String -> Bool -> Element

-- example
checkbox [onToggle Toggled] "Checkbox" model.selected
-- assuming that
data Message = Toggled Bool
```


## Column

```haskell
column :: [Attribute] -> [Element] -> Element

-- example
column [alignX Center, spacing 10] [
  button [onPress Inc] "Increment",
  text [size 50] $ show value,
  button [onPress Dec] "Decrement"
]
```


## ComboBox

```haskell
-- comboBox requires State
newState :: Show option => [option] -> IO State

-- attributes state placeholder selected onSelect
comboBox :: (Show option, Read option)
         => [Attribute option message]
         -> State
         -> String
         -> Maybe option
         -> OnSelect option message
         -> Element

-- example
import Iced.Widget
import Iced.Widget.ComboBox qualified as ComboBox

-- some preparations
data Language = Haskell | Rust deriving (Show, Read)

options :: [Language]
options = [Haskell, Rust]

data Message
  = Selected Language
  | Input String
  | OptionHovered Language
  | Closed

data Model = Model {
  languages :: ComboBox.State,
  selected :: Maybe Language,
  -- some more fields
}

-- comboBox requires state management, you can create it like this
-- and put it into model
languages <- ComboBox.newState options

-- widget usage in view
comboBox [
  width (Fixed 250),
  onInput Input,
  onOptionHovered OptionHovered,
  onClose Closed
] model.languages "Type a language..." model.selected Selected
```


## Container

```haskell
container :: [Attribute] -> Element -> Element

-- example
container [centerX Fill, centerY Fill] $ text [] "Hello"
```


## Image

```haskell
-- There are two ways to create an image. First is from a Handle,
-- which can be either a String path or ByteString file contents.
-- In case of ByteString, iced will guess the file extension
-- on its own
image :: IntoHandle a => [Attribute] -> a -> Element

-- Another one is from pixels, where pixels length must be width * height
-- attributes width height pixels
fromRgba :: [Attribute] -> Int -> Int -> [Word8] -> Element

-- examples
import Iced.Widget
import Iced.Widget.Image qualified as Image

-- From file path
image [] "image.png"


-- From ByteString
-- Read the data and store it into the model
bytes <- ByteString.readFile "image.png"

data Model = Model {
  bytes :: ByteString.ByteString
}

-- and use it in view
image [] model.bytes


-- From pixels
-- Make some pixels
pixels :: [Word8]

-- then use it in view
Image.fromRgba [width (Fixed w), height (Fixed h)] w h pixels
```


## Markdown

```haskell
-- markdown requires State
newState :: String -> IO State
markdown :: State -> Theme -> OnUrlClick message -> Element

-- example
import Iced.Widget
import Iced.Widget.Markdown qualified as Markdown

pageContents :: String

data Message = Click String

data Model = Model {
  state :: Markdown.State
}

-- Make state and put it into model
state <- Markdown.newState pageContents

-- then use it in view
markdown model.state Oxocarbon Click
```


## Text

```haskell
text :: [Attribute] -> String -> Element

-- example
text [size 50] "Welcome"
```
