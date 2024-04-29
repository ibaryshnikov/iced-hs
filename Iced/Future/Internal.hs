module Iced.Future.Internal where

import Control.Monad
import Control.Monad.IO.Class
import Foreign

data NativeFuture a
type FuturePtr a = Ptr (NativeFuture a)

newtype Future a = Future (IO (FuturePtr a))

instance Monad Future where
  (>>=) = compose

instance Functor Future where
  fmap = liftM

instance Applicative Future where
  pure = wrap
  (<*>) = ap

instance MonadIO Future where
  liftIO = wrapIO

foreign export ccall "free_haskell_fun_ptr"
  freeHaskellFunPtr :: FunPtr a -> IO ()

-- wrap STARTS

foreign import ccall "future_wrap_value"
  wrap_value :: StablePtr a -> IO (FuturePtr a)

wrap :: a -> Future a
wrap a = Future $ do
  wrap_value =<< newStablePtr a

-- wrap ENDS

-- compose STARTS

foreign import ccall "future_compose"
  future_compose :: FuturePtr a -> FunPtr (NativeFAB a b) -> IO (FuturePtr b)

type NativeFAB a b = StablePtr a -> IO (FuturePtr b)

foreign import ccall "wrapper"
  makeFABCallback :: NativeFAB a b -> IO (FunPtr (NativeFAB a b))

wrapFAB :: (a -> Future b) -> NativeFAB a b
wrapFAB fab ptrA = do
  a <- fromPtr ptrA
  case fab a of (Future mb) -> mb

compose :: Future a -> (a -> Future b) -> Future b
compose (Future ma) fab = Future $ do
  ptrA <- ma
  fabPtr <- makeFABCallback $ wrapFAB fab
  future_compose ptrA fabPtr

freePtr :: StablePtr a -> IO ()
freePtr ptr = case ptrToIntPtr $ castStablePtrToPtr ptr of
  -- from the docs:
  -- "The StablePtr 0 is reserved for representing NULL in foreign code."
  0 -> pure ()
  _ -> freeStablePtr ptr

fromPtr :: StablePtr a -> IO (a)
fromPtr ptr = do
  value <- deRefStablePtr ptr
  freePtr ptr
  pure value

-- compose ENDS

-- wrapIO STARTS

foreign import ccall "future_wrap_io"
  future_wrap_io :: FunPtr (IO (StablePtr a)) -> IO (FuturePtr a)

foreign import ccall "wrapper"
  makeIOCallback :: IO (StablePtr a) -> IO (FunPtr (IO (StablePtr a)))

wrapIO :: IO a -> Future a
wrapIO io = Future $ wrap_value =<< newStablePtr =<< io

-- wrapIO ENDS

-- concurrent STARTS

foreign import ccall "future_concurrent"
  future_concurrent :: FuturePtr a -> FuturePtr b -> IO (FuturePtr (a, b))

type NativePair a b = StablePtr a -> StablePtr b -> IO (StablePtr (a, b))

foreign export ccall "future_pair"
  pair :: NativePair a b

pair :: NativePair a b
pair ptrA ptrB = do
  a <- fromPtr ptrA
  b <- fromPtr ptrB
  newStablePtr (a, b)

concurrent :: Future a -> Future b -> Future (a, b)
concurrent (Future ma) (Future mb) = Future $ do
  ptrA <- ma
  ptrB <- mb
  future_concurrent ptrA ptrB

-- concurrent ENDS

-- race STARTS

foreign import ccall "future_race"
  future_race :: FuturePtr a -> FuturePtr b -> IO (FuturePtr (Either a b))

type NativeEither a ma = StablePtr a -> IO (StablePtr ma)

foreign export ccall "future_left"  left  :: NativeEither a (Either a b)
foreign export ccall "future_right" right :: NativeEither b (Either a b)

left :: NativeEither a (Either a b)
left = makeEither Left

right :: NativeEither b (Either a b)
right = makeEither Right

makeEither :: (a -> ma) -> NativeEither a ma
makeEither f ptr = do
  a <- fromPtr ptr
  newStablePtr $ f a

race :: Future a -> Future b -> Future (Either a b)
race (Future ma) (Future mb) = Future $ do
  ptrA <- ma
  ptrB <- mb
  future_race ptrA ptrB

-- race ENDS
