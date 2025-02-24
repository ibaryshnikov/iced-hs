{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Iced.Widget.Markdown (
  markdown,
  newState,
  State,
) where

import Control.Monad
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Element
import Iced.Theme

data NativeState
type State = ForeignPtr NativeState

-- contents
foreign import ccall "markdown_state_new"
  state_new :: CString -> IO (Ptr NativeState)

newState :: String -> IO State
newState = newForeignPtr state_free <=< state_new <=< newCString

foreign import ccall "&markdown_state_free"
  state_free :: FinalizerPtr NativeState

-- state theme on_url_click
foreign import ccall "markdown_view"
  markdown_view :: Ptr NativeState -> CUChar -> FunPtr (NativeOnUrlClick a) -> IO ElementPtr

type NativeOnUrlClick message = CString -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnUrlClick message -> IO (FunPtr (NativeOnUrlClick message))

wrapOnUrlClick :: OnUrlClick message -> NativeOnUrlClick message
wrapOnUrlClick callback = newStablePtr . callback <=< peekCString

type OnUrlClick message = String -> message

data Markdown message = Markdown
  { state :: State
  , theme :: Theme
  , onUrlClick :: OnUrlClick message
  }

instance IntoNative (Markdown message) ElementPtr where
  toNative details = do
    onUrlClick <- makeCallback $ wrapOnUrlClick details.onUrlClick
    let theme = fromIntegral $ fromEnum details.theme
    withForeignPtr details.state $ \state ->
      markdown_view state theme onUrlClick

markdown :: State -> Theme -> OnUrlClick message -> Element
markdown state theme onUrlClick = packSimple Markdown{..}
