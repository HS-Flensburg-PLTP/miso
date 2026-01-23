-- | Haskell language pragma
{-# LANGUAGE CPP               #-}
{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.FFI
import Miso.String

-- | JSAddle import
#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp         as Warp
import           Network.WebSockets
#endif
import           Control.Monad.IO.Class

-- | Other imports
import           Data.Kind                        (Type)
import           Data.Maybe                       (catMaybes, fromMaybe)
import           Network.HTTP
import Text.Read (readMaybe)

-- | Type synonym for an application model
newtype CharacterOutline = CharacterOutline
  { _class :: String
  } deriving (Show, Eq)

data FullCharacter = FullCharacter
  { _selectedClass :: String
  , _strength      :: Int
  } deriving (Show, Eq)

data Model a where
  NewCharacter :: CharacterOutline -> Model NewCharacterAction
  CharacterInCreation :: FullCharacter -> Model CharacterInCreationAction
  CreatedCharacter :: FullCharacter -> Model FinalizedCharacterAction

instance Show (Model a) where
  show (NewCharacter char)        = "NewCharacter " ++ show char
  show (CharacterInCreation char) = "CharacterInCreation " ++ show char
  show (CreatedCharacter char)    = "CreatedCharacter " ++ show char

instance Eq (Model a) where
  (NewCharacter char1)        == (NewCharacter char2)        = char1 == char2
  (CharacterInCreation char1) == (CharacterInCreation char2) = char1 == char2
  (CreatedCharacter char1)    == (CreatedCharacter char2)    = char1 == char2

-- | Sum types for application events
data NewCharacterAction
  = NoOp
  | FetchSetClass String
  | SetClass String
  | FetchConfirmClass
  | ConfirmClass
  deriving (Show, Eq)

data CharacterInCreationAction
  = IncrementStrength
  | DecrementStrength
  | FetchRandomStrength
  | SetRandomStrength Int
  | FinalizeCharacter
  deriving (Show, Eq)

data FinalizedCharacterAction
  deriving (Show, Eq)

#ifndef ghcjs_HOST_OS
runApp :: JSM () -> IO ()
runApp f = JSaddle.debugOr 8080 (f >> syncPoint) JSaddle.jsaddleApp
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

-- | Entry point for a miso application
main :: IO ()
main = runApp $ startApp App {..}
  where
    initialAction = NoOp                        -- initial action to be executed on application load
    model  = NewCharacter (CharacterOutline "") -- initial model
    update = updateModel                        -- update function
    view   = viewModel                          -- view function
    events = defaultEvents                      -- default delegated events
    subs   = []                                 -- empty subscription list
    mountPoint = Nothing                        -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                              -- used during prerendering to see if the VDOM and DOM are in sync (only used with `miso` function)

-- | Updates model, optionally introduces side effects
updateModel :: Model action -> action -> AnyEffect Model
updateModel (NewCharacter char) a = case a of
    NoOp -> noEff $ NewCharacter char
    FetchSetClass cls -> NewCharacter char
                         <# do
                           res <- liftIO $ simpleHTTP (getRequest "http://localhost:3456")
                           _ <- consoleLog $ ms (show res)
                           pure (SetClass cls)
    SetClass cls -> noEff
                    $ NewCharacter (char { _class = cls })
    FetchConfirmClass -> NewCharacter char
                         <# do
                           res <- liftIO $ simpleHTTP (getRequest "http://localhost:3456")
                           _ <- consoleLog $ ms (show res)
                           pure ConfirmClass
    ConfirmClass -> noEff
                    $ CharacterInCreation (FullCharacter { _selectedClass = _class char
                                                         , _strength      = 5
                                                         })
updateModel (CharacterInCreation char) a = case a of
    FetchRandomStrength -> CharacterInCreation char
                           <# do
                             res <- liftIO $ simpleHTTP (getRequest "http://localhost:3456/random-attr")
                             _ <- consoleLog $ ms (show res)
                             body <- liftIO $ getResponseBody res
                             pure . SetRandomStrength . fromMaybe (_strength char) . readMaybe $ body
    SetRandomStrength str -> noEff
                             $ CharacterInCreation (char { _strength = str })
    IncrementStrength -> noEff
                         $ CharacterInCreation (char { _strength = _strength char + 1 })
    DecrementStrength -> noEff
                         $ CharacterInCreation (char { _strength = _strength char - 1 })
    FinalizeCharacter -> noEff
                         $ CreatedCharacter char
updateModel (CreatedCharacter char) a = case a of

-- | Constructs a virtual DOM from a model
viewModel :: Model action -> View action
viewModel x =
  div_
    [ class_ "character-creator"
    ]
    (case x of
      NewCharacter char ->
        [ h1_
          []
          [ text "Create Your Character"
          ]
        , input_
          [ type_ "text"
          , placeholder_ "Enter class"
          , value_ (ms (_class char))
          , onInput (FetchSetClass . fromMisoString)
          ]
        , button_
          [ disabled_ (_class char == ""),
            onClick FetchConfirmClass
          ]
          [ text "Confirm Class" ]
        , span_
          []
          [ text (ms (show (Prelude.length (_class char)) ++ " chars"))]
        ]
      CharacterInCreation char ->
        [ h1_
          []
          [ text $ ms (_selectedClass char)
          ]
        , ul_
          []
          [ li_
            []
            [ strong_ [] [ text $ ms ("Strength" ++ ": ") ]
            , button_ [ onClick DecrementStrength ] [ text "-" ]
            , span_ [] [ text $ ms (_strength char) ]
            , button_ [ onClick IncrementStrength ] [ text "+" ]
            , button_ [ onClick FetchRandomStrength ] [ text "Random" ]
            ]
          ]
        , button_
          [ onClick FinalizeCharacter ]
          [ text "Finalize Character" ]
        ]
      CreatedCharacter char ->
        [ h1_
          []
          [ text $ ms (_selectedClass char)
          ]
        , ul_
          []
          [ li_
            []
            [ strong_ [] [ text $ ms ("Strength" ++ ": ") ]
            , span_ [] [ text $ ms (_strength char) ]
            ]
          ]
        ]
    )
