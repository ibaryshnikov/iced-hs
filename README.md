# iced-hs

Haskell wrapper for [iced](https://github.com/iced-rs/iced) gui library.
Very experimental, check [Roadmap](#roadmap) to see progress.


## Platform support

Desktop - Windows, macOS, Linux. Supported targets are the same
as for iced except the web target, since it's tricky to link
wasm produced by different compilers. With some effort it
may be possible, just not out of the box.


## Example

Check [examples](examples) for more

```haskell
import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Theme
import Iced.Widget

data Message = Inc | Dec

update :: Message -> Int -> Int
update Inc = succ
update Dec = pred

view :: Int -> Element
view value =
  container [centerX Fill, centerY Fill] $
  column [alignX Center, spacing 10] [
    button [onPress Inc] "Increment",
    text [size 50] $ show value,
    button [onPress Dec] "Decrement"
  ]

main :: IO ()
main = Iced.run [theme GruvboxLight] "Counter" 0 update view
```

![Counter preview](examples/counter/counter.png)


## Preview

From [themes](examples/themes) example:

![Themes preview](examples/themes/themes.png)


## Usage

First, build rust crate to produce `libiced_hs.a`

```bash
./build_rust.sh
```

then pass it to ghc

```bash
ghc -ipath/to/this/repo path/to/libiced_hs.a main.hs
```


## Internals

- [wgpu](https://github.com/gfx-rs/wgpu) - graphics
- [winit](https://github.com/rust-windowing/winit) - windowing
- [tokio](https://github.com/tokio-rs/tokio) - runtime
- [cosmic-text](https://github.com/pop-os/cosmic-text) - multi-line text handling


## Credits

Below is a list of inspiring projects
- [Elm](https://elm-lang.org/)
- [elm-ui](https://github.com/mdgriffith/elm-ui)
- [elm-canvas](https://github.com/joakin/elm-canvas)
- [webgl](https://github.com/elm-explorations/webgl)
- [miso](https://github.com/dmjio/miso)
- [monomer](https://github.com/fjvallarino/monomer)


## Status

This is a research project. The api may change often.
Not recommended for use in production. It still may be a good
option if you need a simple way to build some ui.


## Roadmap

 - [ ] Widgets
   - [x] button
   - [x] checkbox
   - [x] column
   - [x] comboBox
   - [x] container
   - [x] image
   - [ ] keyedColumn
   - [ ] mouseArea
   - [ ] paneGrid
   - [x] progressBar
   - [x] pickList
   - [ ] qrCode
   - [x] radio
   - [x] responsive
   - [ ] horizontalRule, verticalRule
   - [x] row
   - [x] scrollable
   - [x] slider, verticalSlider
   - [x] horizontalSpace, verticalSpace
   - [ ] svg
   - [x] text
   - [x] textEditor
   - [x] textInput
   - [x] toggler
   - [x] tooltip
 - [ ] Helper functions
   - [ ] lazy
   - [ ] themer
   - [ ] focusNext, focusPrevious
 - [x] Themes
 - [ ] Canvas api
   - [x] canvas widget
   - [x] fill path
   - [x] stroke path
   - [x] path methods - circle, lineTo, moveTo, rectangle
   - [ ] more path methods
 - [ ] Shader
   - [ ] shader widget
   - [ ] attributes
 - [ ] Multi window
 - [ ] Custom widgets
 - [ ] Subscriptions
   - [x] subscription attribute for Application
   - [x] Time.every subscription
   - [ ] Keyboard
     - [x] onKeyPress
     - [x] onKeyRelease
     - [ ] PhysicalKey
       - [x] KeyCode
       - [ ] NativeKeyCode
     - [ ] LogicalKey
       - [x] Named
       - [ ] Character
   - [ ] custom subscriptions
   - [ ] other events
 - [x] Command api
   - [x] `Command.perform` for Rust `Future`
   - [x] `Command.performBlocking` for blocking tasks
 - [ ] Add all attributes from iced for each widget
 - [ ] Styles for widgets
   - [x] button
   - [x] checkobx
   - [x] container
   - [x] pickList
   - [x] progressBar
   - [x] radio
   - [x] text
   - [x] textInput
   - [x] textEditor
   - [ ] ...
