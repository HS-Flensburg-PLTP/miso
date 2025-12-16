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

data AccountInformation = AccountInformation
  { username :: String
  , password :: String
  } deriving (Show, Eq)

data BillingAddressWithAccount = BillingAddressWithAccount
  { account :: AccountInformation
  , billingAddress :: String
  } deriving (Show, Eq)

data ShippingAddress
  = EqualsBillingAddress
  | OtherShippingAddress String
  deriving (Show, Eq)

data ShippingAndBillingAddressWithAccount = ShippingAndBillingAddressWithAccount
  { billingAddressWithAccount :: BillingAddressWithAccount
  , shippingAddress :: ShippingAddress
  } deriving (Show, Eq)

data Model a where
  Intro :: Model IntroAction
  AddAccountInformation :: AccountInformation -> Model AddAccountInformationAction
  AddBillingAddress :: BillingAddressWithAccount -> Model AddBillingAddressAction
  AddShippingAddress :: ShippingAndBillingAddressWithAccount -> Model AddShippingAddressAction
  Check :: ShippingAndBillingAddressWithAccount -> Model CheckAction
  OrderConfirmed :: ShippingAndBillingAddressWithAccount -> Model OrderConfirmedAction

instance Show (Model a) where
  show Intro                        = "Intro"
  show (AddAccountInformation char) = "AddAccountInformation " ++ show char
  show (AddBillingAddress char)     = "AddBillingAddress " ++ show char
  show (AddShippingAddress char)    = "AddShippingAddress " ++ show char
  show (Check char)                 = "Check " ++ show char
  show (OrderConfirmed char)        = "OrderConfirmed " ++ show char

instance Eq (Model a) where
  Intro                          == Intro                          = True
  (AddAccountInformation state1) == (AddAccountInformation state2) = state1 == state2
  (AddBillingAddress state1)     == (AddBillingAddress state2)     = state1 == state2
  (AddShippingAddress state1)    == (AddShippingAddress state2)    = state1 == state2
  (Check state1)                 == (Check state2)                 = state1 == state2
  (OrderConfirmed state1)        == (OrderConfirmed state2)        = state1 == state2

-- | Sum types for application events
data IntroAction
  = NoOp
  | Checkout
  deriving (Show, Eq)

data AddAccountInformationAction
  = SetUsername String
  | SetPassword String
  | ToBillingAddress
  deriving (Show, Eq)

data AddBillingAddressAction
  = SetBillingAddress String
  | ToShippingAddress
  deriving (Show, Eq)

data AddShippingAddressAction
  = SetShippingAddress String
  | DeriveFromBillingAddress
  | ToCheck
  deriving (Show, Eq)

data CheckAction
  = ConfirmOrder
  deriving (Show, Eq)

data OrderConfirmedAction
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
    initialAction = NoOp          -- initial action to be executed on application load
    model         = Intro         -- initial model
    update        = updateModel   -- update function
    view          = viewModel     -- view function
    events        = defaultEvents -- default delegated events
    subs          = []            -- empty subscription list
    mountPoint    = Nothing       -- mount point for application (Nothing defaults to 'body')
    logLevel      = Off           -- used during prerendering to see if the VDOM and DOM are in sync (only used with `miso` function)

-- | Updates model, optionally introduces side effects
updateModel :: Model action -> action -> Effect action (AnyModel Model)
updateModel Intro a = noEff $ case a of
  NoOp     -> AnyModel Intro
  Checkout -> AnyModel $ AddAccountInformation $ AccountInformation { username = "", password = "" }
updateModel (AddAccountInformation accInfo) a = noEff $ case a of
  SetUsername newUsername -> AnyModel $ AddAccountInformation $ accInfo { username = newUsername }
  SetPassword newPassword -> AnyModel $ AddAccountInformation $ accInfo { password = newPassword }
  ToBillingAddress        -> AnyModel $ AddBillingAddress $ BillingAddressWithAccount { account = accInfo, billingAddress = "" }
updateModel (AddBillingAddress orderInfo) a = noEff $ case a of
  SetBillingAddress newAddress -> AnyModel $ AddBillingAddress $ orderInfo { billingAddress = newAddress }
  ToShippingAddress            -> AnyModel $ AddShippingAddress $ ShippingAndBillingAddressWithAccount { billingAddressWithAccount = orderInfo, shippingAddress = EqualsBillingAddress }
updateModel (AddShippingAddress orderInfo) a = noEff $ case a of
  SetShippingAddress newAddress -> AnyModel $ AddShippingAddress $ orderInfo { shippingAddress = OtherShippingAddress newAddress }
  DeriveFromBillingAddress      -> AnyModel $ AddShippingAddress $ orderInfo { shippingAddress = EqualsBillingAddress }
  ToCheck                       -> AnyModel $ Check orderInfo
updateModel (Check orderInfo) a = noEff $ case a of
  ConfirmOrder -> AnyModel (OrderConfirmed orderInfo)
updateModel (OrderConfirmed orderInfo) a = noEff $ case a of

-- | Constructs a virtual DOM from a model
viewModel :: Model action -> View action
viewModel m =
  div_
    [ class_ "character-creator"
    ]
    (case m of
      Intro ->
        [ h1_
          []
          [ text "Order your item"
          ]
        , p_
          []
          [ text "We need a couple of details from you to ship the item." ]
        , button_
          [ onClick Checkout
          ]
          [ text "Start Checkout Process"
          ]
        ]
      AddAccountInformation accInfo ->
        [ h1_
          []
          [ text "Add account information"
          ]
        , input_
          [ type_ "text"
          , placeholder_ "Username"
          , value_ $ ms $ username accInfo
          , onInput (SetUsername . fromMisoString)
          ]
        , input_
          [ type_ "password"
          , placeholder_ "Password"
          , value_ $ ms $ username accInfo
          , onInput (SetUsername . fromMisoString)
          ]
        , button_
          [ onClick ToBillingAddress ]
          [ text "Continue" ]
        ]
      AddBillingAddress orderInfo ->
        [ h1_
          []
          [ text "Add billing address"
          ]
        , input_
          [ type_ "text"
          , placeholder_ "Address"
          , value_ $ ms $ billingAddress orderInfo
          , onInput (SetBillingAddress . fromMisoString)
          ]
        , button_
          [ onClick ToShippingAddress ]
          [ text "Continue" ]
        ]
      AddShippingAddress orderInfo ->
        catMaybes [ Just $ h1_
                    []
                    [ text "Add shipping address"
                    ]
                  , Just $ fieldset_
                    []
                    [ input_
                      [ type_ "radio"
                      , name_ "shippingAddressType"
                      , value_ "derive"
                      , onInput (const DeriveFromBillingAddress)
                      , checked_ $ case shippingAddress orderInfo of
                          EqualsBillingAddress -> True
                          OtherShippingAddress _ -> False
                      ]
                    , input_
                      [ type_ "radio"
                      , name_ "shippingAddressType"
                      , value_ "other"
                      , onInput (const $ SetShippingAddress "")
                      , checked_ $ case shippingAddress orderInfo of
                          EqualsBillingAddress -> False
                          OtherShippingAddress _ -> True
                      ]
                    ]
                  , case shippingAddress orderInfo of
                      EqualsBillingAddress -> Nothing
                      OtherShippingAddress otherShippingAddress ->
                        Just $ input_
                                 [ type_ "text"
                                 , placeholder_ "Address"
                                 , value_ $ ms otherShippingAddress
                                 , onInput (SetShippingAddress . fromMisoString)
                                 ]
                  , Just $ button_
                    [ onClick ToCheck ]
                    [ text "Check" ]
                  ]
      Check orderInfo ->
        [ h1_
          []
          [ text "Check information"
          ]
        , dl_
          []
          [ dt_ [] [ text "Username" ]
          , dd_ [] [ text $ ms $ username $ account $ billingAddressWithAccount orderInfo ]
          , dt_ [] [ text "BillingAddress" ]
          , dd_ [] [ text $ ms $ billingAddress $ billingAddressWithAccount orderInfo ]
          , dt_ [] [ text "Shipping Address" ]
          , dd_ [] [ case shippingAddress orderInfo of
              EqualsBillingAddress -> em_ [] [ text "Equals billing address" ]
              OtherShippingAddress addr -> text $ ms addr
            ]
          ]
        , button_
          [ onClick ConfirmOrder ]
          [ text "Confirm Order" ]
        ]
      OrderConfirmed char ->
        [ h1_
          []
          [ text "Order Confirmed"
          ]
        , p_
          []
          [ text "Your item will arrive soon." ]
        ]
    )
