-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

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
import Data.Maybe (catMaybes)

-- | Type synonym for an application model
newtype CharacterOutline = CharacterOutline
  { _class :: String
  } deriving (Show, Eq)

data FullCharacter = FullCharacter
  { _selectedClass :: String
  , _strength      :: Int
  } deriving (Show, Eq)

data Model
  = NewCharacter CharacterOutline
  | CharacterInCreation FullCharacter
  | CreatedCharacter FullCharacter
  deriving (Show, Eq)

-- | Sum type for application events
data Action
  = SetClass String
  | ConfirmClass
  | IncrementStrength
  | DecrementStrength
  | FinalizeCharacter
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
updateModel :: Action -> Model -> Effect Action Model
updateModel (SetClass cls) m = noEff (case m of
    NewCharacter char -> NewCharacter (char { _class = cls })
    x                 -> x)
updateModel ConfirmClass m = noEff (case m of
    NewCharacter char -> CharacterInCreation (FullCharacter { _selectedClass = _class char
                                                            , _strength      = 5
                                                            })
    x                 -> x)
updateModel IncrementStrength m = noEff (case m of
    CharacterInCreation char -> CharacterInCreation (char { _strength = _strength char + 1 })
    x                        -> x)
updateModel DecrementStrength m = noEff (case m of
    CharacterInCreation char -> CharacterInCreation (char { _strength = _strength char - 1 })
    x                        -> x)
updateModel FinalizeCharacter m = noEff (case m of
    CharacterInCreation char -> CreatedCharacter char
    x                        -> x)

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
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
