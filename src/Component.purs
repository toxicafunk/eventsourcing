module Component (State, Query(..), ui) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX

type State =
  { loading :: Boolean
  , eventId :: String
  , result :: Maybe String
  }

data Query a
  = SetEventId String a
  | MakeRequest a

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AX.AJAX | eff))
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { loading: false, eventId: "", result: Nothing }

  render :: State -> H.ComponentHTML Query
  render st =
    HH.form_ $
      [ HH.h1_ [ HH.text "Lookup event id" ]
      , HH.label_
          [ HH.div_ [ HH.text "Enter eventId:" ]
          , HH.input
              [ HP.value st.eventId
              , HE.onValueInput (HE.input SetEventId)
              ]
          ]
      , HH.button
          [ HP.disabled st.loading
          , HE.onClick (HE.input_ MakeRequest)
          ]
          [ HH.text "Fetch info" ]
      , HH.p_
          [ HH.text (if st.loading then "Working..." else "") ]
      , HH.div_
          case st.result of
            Nothing -> []
            Just res ->
              [ HH.h2_
                  [ HH.text "Response:" ]
              , HH.pre_
                  [ HH.code_ [ HH.text res ] ]
              ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AX.AJAX | eff))
  eval = case _ of
    SetEventId eventId next -> do
      H.modify (_ { eventId = eventId, result = Nothing :: Maybe String })
      pure next
    MakeRequest next -> do
      eventId <- H.gets _.eventId
      H.modify (_ { loading = true })
      response <- H.liftAff $ AX.get ("http://localhost:9090/events/" <> eventId)
      H.modify (_ { loading = false, result = Just response.response })
      pure next
