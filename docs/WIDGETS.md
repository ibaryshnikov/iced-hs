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
-- clear the cache when you want a redraw
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
import Iced.Theme
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


## PickList

```haskell
-- attributes options selected onSelect
pickList :: (Show option, Read option)
         => [Attribute]
         -> [option]
         -> Maybe option
         -> OnSelect option message
         -> Element

-- example
import Iced.Widget

-- prepare options
data Language = Haskell | Rust deriving (Show, Read)

data Message = Selected Language

data Model = Model { selected :: Maybe Language }

options :: [Language]
options = [Haskell, Rust]

-- use widget in view
pickList [placeholder "Type a language..."] options model.selected Selected
```


## ProgressBar

```haskell
-- attributes rangeFrom rangeTo value
progressBar :: [Attribute] -> Float -> Float -> Float -> Element

-- example
import Iced.Widget
import Iced.Widget.ProgressBar qualified as ProgressBar

-- use in view
progressBar [width $ Fixed 300, height $ Fixed 30, style ProgressBar.Primary] 0 100 model.value,
```


## Radio

```haskell
-- attributes label value selected onClick
radio :: Enum option
      => [Attribute]
      -> String
      -> option
      -> Maybe option
      -> OnClick option message
      -> Element

-- example
import Iced.Widget

data Option = First | Second deriving Enum

data Message = Selected Option

data Model = Model { selected :: Maybe Option }

-- use in view
row [] [
  radio [] "First" First model.selected Selected,
  radio [] "Second" Second model.selected Selected
]
```


## Responsive

Complete [example](../examples/responsive)

```haskell
data Size = Size { width :: Float, height :: Float }
type View = Size -> Element

responsive :: View -> Element

-- example
import Iced.Size
import Iced.Widget

-- Define a function which accepts Size and returns a widget
label :: Size -> Element
label s = column [] $ if s.width < 200 then [] else [
  text [] "Responsive widget size",
  text [] $ "width   " ++ show s.width,
  text [] $ "height  " ++ show s.height
]

-- then use it in view
responsive label
```


## Row

```haskell
row :: [Attribute] -> [Element] -> Element

-- example
import Iced.Widget

-- use in view
row [alignY Center, spacing 10] [
  button [onPress Inc] "Increment",
  text [size 50] $ show value,
  button [onPress Dec] "Decrement"
]
```


## Scrollable

```haskell
scrollable :: [Attribute] -> Element -> Element

-- example
import Iced.Widget

-- use in view
scrollable [] $
column [width Fill, alignX Center, spacing 10] [
  -- space is needed to fill the screen
  -- so we can have something to scroll
  spaceHeight (Fixed 600),
  text [] "Some text",
  spaceHeight (Fixed 600)
]
```


## Slider

```haskell
-- attributes rangeFrom rangeTo value onChange
slider :: [Attribute message] -> Int -> Int -> Int -> OnChange message -> Element
verticalSlider :: [Attribute message] -> Int -> Int -> Int -> OnChange message -> Element

-- example
import Iced.Widget

-- use in view
-- note that for horizontal slider there's width
slider [width (Fixed 150), onRelease Released] 0 100 value Changed
-- and height for vertical
verticalSlider [height (Fixed 150), onRelease Released] 0 100 value Changed
```


## Space

```haskell
data Length = Fill | FillPortion Word16 | Shrink | Fixed Float

-- width height
space :: Length -> Length -> Element
-- space with given width
spaceWidth :: Length -> Element
-- space with given height
spaceHeight :: Length -> Element
-- fill horizontal space
horizontalSpace :: Element
-- fill vertical space
verticalSpace :: Element

-- example
import Iced.Widget

-- use in view
spaceHeight (Fixed 600)
```


## Text

```haskell
text :: [Attribute] -> String -> Element

-- example
text [size 50] "Welcome"
```


## TextEditor

```haskell
textEditor :: [Attribute message] -> Content -> Element

-- textEditor requires Content
-- create empty Content
newContent :: IO Content
-- create Content from given String
contentWithText :: String -> IO Content
-- perform editor Action on Content
perform :: Content -> Action -> IO ()

-- example
import Iced.Widget
import Iced.Widget.TextEditor qualified as TextEditor

data Message = EditorAction TextEditor.Action

data Model = Model { content :: TextEditor.Content }

-- perform editor Action in update function
update :: Message -> Model -> IO Model
update (EditorAction action) model = do
  TextEditor.perform model.content action
  pure model

-- use in view
textEditor [height (Fixed 300), onAction EditorAction] model.content

-- create content
main :: IO ()
main = do
  content <- TextEditor.newContent
  let model = Model { content = content }
  Iced.run [] "TextEditor" model update view
```
