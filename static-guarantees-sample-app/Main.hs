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
import           Data.Maybe                       (catMaybes)

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
  = SetClass String
  | ConfirmClass
  deriving (Show, Eq)

data CharacterInCreationAction
  = IncrementStrength
  | DecrementStrength
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
    initialAction = SetClass ""                 -- initial action to be executed on application load
    model  = NewCharacter (CharacterOutline "") -- initial model
    update = updateModel                        -- update function
    view   = viewModel                          -- view function
    events = defaultEvents                      -- default delegated events
    subs   = []                                 -- empty subscription list
    mountPoint = Nothing                        -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                              -- used during prerendering to see if the VDOM and DOM are in sync (only used with `miso` function)

-- | Updates model, optionally introduces side effects
updateModel :: Model action -> action -> Effect action (AnyModel Model)
updateModel (NewCharacter char) a = noEff $ case a of
    SetClass cls -> AnyModel $ NewCharacter $ char { _class = cls }
    ConfirmClass -> AnyModel $ CharacterInCreation $ FullCharacter { _selectedClass = _class char
                                                                   , _strength      = 5
                                                                   }
updateModel (CharacterInCreation char)  a = noEff $ case a of
    IncrementStrength -> AnyModel $ CharacterInCreation $ char { _strength = _strength char + 1 }
    DecrementStrength -> AnyModel $ CharacterInCreation $ char { _strength = _strength char - 1 }
    FinalizeCharacter -> AnyModel $ CreatedCharacter char
updateModel (CreatedCharacter char) a = noEff $ case a of

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
          , onInput (SetClass . fromMisoString)
          ]
        , button_
          [ disabled_ (_class char == ""),
            onClick ConfirmClass
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
