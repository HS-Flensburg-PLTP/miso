-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Types
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
-- | Haskell language pragma
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Miso.Types
  ( App (..)
  , AnyModel (..)
  , LogLevel (..)
  , Effect
  , Sub

    -- * The Transition Monad
  , Transition
  , mapAction
  , fromTransition
  , toTransition
  , scheduleIO
  , scheduleIO_
  , scheduleIOFor_
  , scheduleSub
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict (StateT(StateT), execStateT, mapStateT)
import           Control.Monad.Trans.Writer.Strict (WriterT(WriterT), Writer, runWriter, tell, mapWriter)
import           Data.Bifunctor (second)
import           Data.Foldable (for_)
import qualified Data.Map as M
import           Miso.Effect
import           Miso.FFI (JSM)
import           Miso.Html.Types (View)
import           Miso.String
import           Data.Typeable

-- | Application entry point
data App model currentModelAction = App
  { model :: model currentModelAction
  -- ^ initial model
  , update :: forall action. model action -> action -> AnyEffect model
  -- ^ Function to update model, optionally providing effects.
  --   See the 'Transition' monad for succinctly expressing model transitions.
  , view :: forall action. model action -> View action
  -- ^ Function to draw `View`
  , subs :: [ Sub currentModelAction ]
  -- ^ List of subscriptions to run during application lifetime
  , events :: M.Map MisoString Bool
  -- ^ List of delegated events that the body element will listen for.
  --   You can start with 'Miso.Event.Types.defaultEvents' and modify as needed.
  , initialAction :: currentModelAction
  -- ^ Initial action that is run after the application has loaded
  , mountPoint :: Maybe MisoString
  -- ^ Id of the root element for DOM diff. If 'Nothing' is provided, the entire document body is used as a mount point.
  , logLevel :: LogLevel
  -- ^ Display warning messages when prerendering if the DOM and VDOM are not in sync.
  }

-- | A wrapper to hold any model type
data AnyModel m where
  AnyModel :: (Typeable a, Eq (m a), Show (m a), Show a) => m a -> AnyModel m

-- instance Eq (AnyModel m) where
--   (AnyModel m1) == (AnyModel m2) =
--     let actualModel1 = concreteModel (AnyModel m1)
--         actualModel2 = concreteModel (AnyModel m2)
--         equalType = typeOf actualModel1 == typeOf actualModel2
--     in case (actualModel1, actualModel2) of
--          (Just am1, Just am2) -> case cast (am1 :: am2) of
--                                    Just am2' -> am1 == am2'
--                                    Nothing   -> False
--          _                    -> False

-- -- | Attempt to cast an 'AnyModel' back to its concrete type
-- concreteModel :: (Typeable m, Typeable a) => AnyModel m -> Maybe (m a)
-- concreteModel (AnyModel m) = gcast m

-- | Optional Logging for debugging miso internals (useful to see if prerendering is successful)
data LogLevel
  = Off
  | DebugPrerender
  deriving (Show, Eq)

-- | A monad for succinctly expressing model transitions in the 'update' function.
--
-- @Transition@ is a state monad so it abstracts over manually passing the model
-- around. It's also a writer monad where the accumulator is a list of scheduled
-- IO actions. Multiple actions can be scheduled using
-- @Control.Monad.Writer.Class.tell@ from the @mtl@ library and a single action
-- can be scheduled using 'scheduleIO'.
--
-- Tip: use the @Transition@ monad in combination with the stateful
-- <http://hackage.haskell.org/package/lens-4.15.4/docs/Control-Lens-Operators.html lens>
-- operators (all operators ending in "@=@"). The following example assumes
-- the lenses @field1@, @counter@ and @field2@ are in scope and that the
-- @LambdaCase@ language extension is enabled:
--
-- @
-- myApp = App
--   { update = 'fromTransition' . \\case
--       MyAction1 -> do
--         field1 .= value1
--         counter += 1
--       MyAction2 -> do
--         field2 %= f
--         scheduleIO $ do
--           putStrLn \"Hello\"
--           putStrLn \"World!\"
--   , ...
--   }
-- @
type Transition action model = StateT model (Writer [Sub action])

-- | Turn a transition that schedules subscriptions that consume
-- actions of type @a@ into a transition that schedules subscriptions
-- that consume actions of type @b@ using the supplied function of
-- type @a -> b@.
mapAction :: (actionA -> actionB) -> Transition actionA model r -> Transition actionB model r
mapAction = mapStateT . mapWriter . second . fmap . mapSub

-- | Convert a @Transition@ computation to a function that can be given to 'update'.
fromTransition
    :: Transition action (model action) ()
    -> (model action -> Effect action model) -- ^ model 'update' function.
fromTransition act = uncurry Effect . runWriter . execStateT act

-- | Convert an 'update' function to a @Transition@ computation.
toTransition
    :: (model action -> Effect action model) -- ^ model 'update' function
    -> Transition action (model action) ()
toTransition f = StateT $ \s ->
                   let Effect s' ios = f s
                   in WriterT $ pure (((), s'), ios)

-- | Schedule a single IO action for later execution.
--
-- Note that multiple IO action can be scheduled using
-- @Control.Monad.Writer.Class.tell@ from the @mtl@ library.
scheduleIO :: JSM action -> Transition action model ()
scheduleIO ioAction = scheduleSub $ \sink -> ioAction >>= liftIO . sink

-- | Like 'scheduleIO' but doesn't cause an action to be dispatched to
-- the 'update' function.
--
-- This is handy for scheduling IO computations where you don't care
-- about their results or when they complete.
scheduleIO_ :: JSM () -> Transition action model ()
scheduleIO_ ioAction = scheduleSub $ const ioAction

-- | Like `scheduleIO_` but generalized to any instance of `Foldable`
--
-- This is handy for scheduling IO computations that return a `Maybe` value
scheduleIOFor_ :: Foldable f => JSM (f action) -> Transition action model ()
scheduleIOFor_ io = scheduleSub $ \sink -> io >>= \m -> liftIO (for_ m sink)

-- | Like 'scheduleIO' but schedules a subscription which is an IO
-- computation that has access to a 'Sink' which can be used to
-- asynchronously dispatch actions to the 'update' function.
--
-- A use-case is scheduling an IO computation which creates a
-- 3rd-party JS widget which has an associated callback. The callback
-- can then call the sink to turn events into actions. To do this
-- without accessing a sink requires going via a @'Sub'scription@
-- which introduces a leaky-abstraction.
scheduleSub :: Sub action -> Transition action model ()
scheduleSub sub = lift $ tell [ sub ]
