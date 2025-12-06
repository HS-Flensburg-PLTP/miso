{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

#ifdef IOS
#else
{-# LANGUAGE TemplateHaskell     #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso
  ( miso
  , startApp
  , sink
  , module Miso.Effect
  , module Miso.Event
  , module Miso.Html
  , module Miso.Subscription

  , module Miso.TypeLevel

  , module Miso.Types
  , module Miso.Router
  , module Miso.Util
  , module Miso.FFI
  , module Miso.WebSocket
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Dynamic
import           Data.IORef
import           Data.List
import           Data.Sequence                 ((|>))
import qualified Data.Sequence                 as S
import           Data.Typeable
import qualified JavaScript.Object.Internal    as OI
import           System.IO.Unsafe
import           System.Mem.StableName

#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle   (eval, waitForAnimationFrame)
#ifdef IOS
import           Miso.JSBits
#else
import           GHCJS.Types                   (JSString)
import           Data.FileEmbed
#endif
#else
import           JavaScript.Web.AnimationFrame
#endif

import           Miso.Concurrent
import           Miso.Delegate
import           Miso.Diff
import           Miso.Effect
import           Miso.Event
import           Miso.FFI
import           Miso.Html
import           Miso.Router
import           Miso.Subscription
#ifndef ghcjs_HOST_OS
import           Miso.TypeLevel
#endif
import           Miso.Types
import           Miso.Util
import           Miso.WebSocket

-- | Helper function to abstract out common functionality between `startApp` and `miso`
common
  :: forall model action. (Typeable model, Typeable action, Eq (model action), Eq action) => Eq (model action) -- TODO: warum explizit eingeführt (scoped type variables)?
  => App model action
  -> model action
  -> (Sink action -> JSM (IORef VTree))
  -> JSM ()
common App {..} m getView = do
#ifndef ghcjs_HOST_OS
#ifdef IOS
  mapM_ eval [delegateJs,diffJs,isomorphicJs,utilJs]
#else
  _ <- eval ($(embedStringFile "jsbits/delegate.js") :: JSString)
  _ <- eval ($(embedStringFile "jsbits/diff.js") :: JSString)
  _ <- eval ($(embedStringFile "jsbits/isomorphic.js") :: JSString)
  _ <- eval ($(embedStringFile "jsbits/util.js") :: JSString)
#endif
#endif
  -- init Notifier
  Notify {..} <- liftIO newNotify
  -- init empty actions
  actionsRef <- liftIO (newIORef (S.empty :: S.Seq Dynamic))
  let writeEvent a = void . liftIO . forkIO $ do
        atomicModifyIORef' actionsRef $ \as -> (as |> toDyn a, ())
        notify
  -- init global sink
  liftIO (writeIORef (sinkRef :: IORef (Sink action)) writeEvent)
  -- init Subs
  forM_ subs $ \sub ->
    sub writeEvent
  -- Hack to get around `BlockedIndefinitelyOnMVar` exception
  -- that occurs when no event handlers are present on a template
  -- and `notify` is no longer in scope
  void . liftIO . forkIO . forever $ threadDelay (1000000 * 86400) >> notify
  -- Retrieves reference view
  viewRef <- getView writeEvent
  -- know thy mountElement
  mountEl <- mountElement mountPoint
  -- Begin listening for events in the virtual dom
  delegator mountEl viewRef events
  -- Process initial action of application
  writeEvent initialAction
  -- Program loop, blocking on SkipChan

  let
    loop :: forall action'. (Typeable action', Eq (model action')) => model action' -> JSM ()
    loop !oldModel = liftIO wait >> do
        -- Apply actions to model
        actions <- liftIO $ atomicModifyIORef' actionsRef $ \actions -> (S.empty, actions)
        -- let (Acc anyNewModel effects) = foldl' (foldEffects writeEvent update)
        --                                     (Acc (AnyModel oldModel) (pure ())) actions
        case foldl' (foldEffects writeEvent update)
                    (Acc (AnyModel oldModel) (pure ())) actions of
          (Acc (AnyModel newModel) effects) -> do -- TODO: let vs case bei existenziellen Typen

            effects
            oldName <- liftIO $ oldModel `seq` makeStableName oldModel
            newName <- liftIO $ newModel `seq` makeStableName newModel
            when ({- oldName /= newName && -} eqModel oldModel newModel) $ do
              swapCallbacks
              oldVTree <- liftIO (readIORef viewRef)
              newVTree <- runView (view newModel) writeEvent
              void waitForAnimationFrame
              diff mountPoint (Just oldVTree) (Just newVTree)
              releaseCallbacks
              liftIO (atomicWriteIORef viewRef newVTree)
            syncPoint
            loop newModel
  loop m

eqModel :: forall m a1 a2. (Typeable a1, Typeable a2, Eq (m a1)) => m a1 -> m a2 -> Bool
eqModel m1 m2 = case gcast m2 :: Maybe (m a1) of
  Just m2' -> m1 == m2'
  Nothing  -> False

-- | Runs an isomorphic miso application.
-- Assumes the pre-rendered DOM is already present
miso :: (Typeable model, Typeable action, Eq (model action), Eq action) => (URI -> App model action) -> JSM ()
miso f = do
  app@App {..} <- f <$> getCurrentURI
  common app model $ \writeEvent -> do
    let initialView = view model
    VTree (OI.Object iv) <- runView initialView writeEvent
    mountEl <- mountElement mountPoint
    -- Initial diff can be bypassed, just copy DOM into VTree
    copyDOMIntoVTree (logLevel == DebugPrerender) mountEl iv
    let initialVTree = VTree (OI.Object iv)
    -- Create virtual dom, perform initial diff
    liftIO (newIORef initialVTree)

sinkRef :: IORef (Sink action)
{-# NOINLINE sinkRef #-}
sinkRef = unsafePerformIO $ newIORef (\_ -> pure ())

-- | Global sink exposed as a backdoor
-- Meant for usage in long running IO actions, or custom callbacks
-- Good for integrating with third-party components.
sink :: Sink action
sink = unsafePerformIO (readIORef sinkRef)

-- | Runs a miso application
startApp :: (Typeable model, Typeable action, Eq (model action), Eq action) => App model action -> JSM ()
startApp app@App {..} =
  common app model $ \writeEvent -> do
    let initialView = view model
    initialVTree <- runView initialView writeEvent
    diff mountPoint Nothing (Just initialVTree)
    liftIO (newIORef initialVTree)

-- | Helper
foldEffects
  :: forall model action. (Typeable model, Typeable action) => Sink action
  -> (model action -> action -> Effect action (AnyModel model))
  -> Acc (AnyModel model) -> action -> Acc (AnyModel model)
foldEffects snk update (Acc anyModel as) action =
  case anyModel of
    AnyModel model ->
      case gcast model :: Maybe (model action) of
        Nothing -> Acc anyModel as
        Just m ->
          case update m action of
            Effect newModel effs -> Acc newModel newAs
              where
                newAs = as >> do
                  forM_ effs $ \eff -> forkJSM (eff snk)

data Acc model = Acc !model !(JSM ())
