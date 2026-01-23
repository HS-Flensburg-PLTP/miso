{-# LANGUAGE ExistentialQuantification #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Effect
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines `Effect` and `Sub` types, which are used to define
-- `Miso.Types.update` function and `Miso.Types.subs` field of the `Miso.Types.App`.
----------------------------------------------------------------------------
module Miso.Effect (
  module Miso.Effect.Storage
, module Miso.Effect.DOM
, Effect (..), AnyEffect (..), Sub, Sink
, mapSub
, noEff
, (<#)
, (#>)
, batchEff
, effectSub
) where

import Data.Bifunctor

import Control.Monad.IO.Class
import Miso.FFI (JSM)

import Miso.Effect.Storage
import Miso.Effect.DOM
import Data.Typeable (Typeable)

-- | An effect represents the results of an update action.
--
-- It consists of the updated model and a list of subscriptions. Each 'Sub' is
-- run in a new thread so there is no risk of accidentally blocking the
-- application.
data Effect action model = Effect (model action) [Sub action]

data AnyEffect model = forall action. (Show action, Show (model action), Eq (model action), Typeable action) => AnyEffect (Effect action model)

-- | Type synonym for constructing event subscriptions.
--
-- The 'Sink' callback is used to dispatch actions which are then fed
-- back to the 'Miso.Types.update' function.
type Sub action = Sink action -> JSM ()

-- | Function to asynchronously dispatch actions to the 'Miso.Types.update' function.
type Sink action = action -> IO ()

-- | Turn a subscription that consumes actions of type @a@ into a subscription
-- that consumes actions of type @b@ using the supplied function of type @a -> b@.
mapSub :: (actionA -> actionB) -> Sub actionA -> Sub actionB
mapSub f sub sinkB = let sinkA = sinkB . f
                     in sub sinkA

-- instance Functor (Effect action) where
--   fmap f (Effect m acts) = Effect (f m) acts

-- instance Applicative (Effect action) where
--   pure m = Effect m []
--   Effect fModel fActs <*> Effect xModel xActs = Effect (fModel xModel) (fActs ++ xActs)

-- instance Monad (Effect action) where
--   return = pure
--   Effect m acts >>= f =
--     case f m of
--       Effect m' acts' -> Effect m' (acts ++ acts')

-- instance Bifunctor Effect where
--   bimap f g (Effect m acts) = Effect (g m) (map (\act -> \sink -> act (sink . f)) acts)

-- | Smart constructor for an 'Effect' with no actions.
noEff :: (Show action, Show (model action), Eq (model action), Typeable action) => model action -> AnyEffect model
noEff m = AnyEffect (Effect m [])

-- | Smart constructor for an 'Effect' with exactly one action.
(<#) :: (Show action, Show (model action), Eq (model action), Typeable action) => model action -> JSM action -> AnyEffect model
(<#) m a = effectSub m $ \sink -> a >>= liftIO . sink

-- | `Effect` smart constructor, flipped
(#>) :: (Show action, Show (model action), Eq (model action), Typeable action) => JSM action -> model action -> AnyEffect model
(#>) = flip (<#)

-- | Smart constructor for an 'Effect' with multiple actions.
batchEff :: (Show action, Show (model action), Eq (model action), Typeable action) => model action -> [JSM action] -> AnyEffect model
batchEff model actions = AnyEffect $ Effect model $
  map (\a sink -> liftIO . sink =<< a) actions

-- | Like '<#' but schedules a subscription which is an IO computation which has
-- access to a 'Sink' which can be used to asynchronously dispatch actions to
-- the 'Miso.Types.update' function.
--
-- A use-case is scheduling an IO computation which creates a 3rd-party JS
-- widget which has an associated callback. The callback can then call the sink
-- to turn events into actions. To do this without accessing a sink requires
-- going via a @'Sub'scription@ which introduces a leaky-abstraction.
effectSub :: (Show action, Show (model action), Eq (model action), Typeable action) => model action -> Sub action -> AnyEffect model
effectSub model sub = AnyEffect (Effect model [sub])
