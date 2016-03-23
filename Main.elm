module Main (..) where

import Html exposing (div, button, text, toElement, input, hr)
import Html.Events exposing (onClick, on, targetValue)
import Html.Attributes exposing (style, value, placeholder, type', value, min, max)
import StartApp
import Signal
import Effects exposing (Never)
import Task exposing (..)
import String
import Time exposing (..)


app : StartApp.App Model
app =
  StartApp.start { init = init, view = view, update = update, inputs = [ Signal.map clock (every Time.second) ] }


main : Signal Html.Html
main =
  app.html


clock : a -> Action
clock t =
  IncreaseSecs


init : ( Model, Effects.Effects a )
init =
  ( initialModel, Effects.none )


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks


type alias Model =
  { x : Int
  , y : Int
  , text : String
  , secs : Time
  }


initialModel : Model
initialModel =
  { x = 0, y = 0, text = "Default", secs = 0 }


type Action
  = IncrementX
  | DecrementX
  | IncrementY
  | DecrementY
  | ReverseString String
  | IncreaseSecs


update : Action -> Model -> ( Model, Effects.Effects a )
update action model =
  let
    debug =
      Debug.log "was ist hier los" model
  in
    case action of
      IncrementX ->
        ( { model | x = model.x + 1 }
        , Effects.none
        )

      DecrementX ->
        ( { model | x = model.x - 1 }
        , Effects.none
        )

      IncrementY ->
        ( { model | y = model.y + 1 }
        , Effects.none
        )

      DecrementY ->
        ( { model | y = model.y - 1 }
        , Effects.none
        )

      ReverseString str ->
        ( { model | text = String.reverse str }
        , Effects.none
        )

      IncreaseSecs ->
        if model.secs > 60 then
          ( { model | secs = 1 }
          , Effects.none
          )
        else
          ( { model | secs = model.secs + 1 }
          , Effects.none
          )


view : Signal.Address Action -> Model -> Html.Html
view address model =
  div
    []
    [ button
        [ onClick address IncrementX ]
        [ Html.text "+ X" ]
    , button
        [ onClick address DecrementX ]
        [ Html.text "- X" ]
    , button
        [ onClick address IncrementY ]
        [ Html.text "+ Y" ]
    , button
        [ onClick address DecrementY ]
        [ Html.text "- Y" ]
    , div
        []
        [ Html.text ("X: " ++ toString model.x)
        , Html.text (" ---- Y: " ++ toString model.y)
        ]
    , hr
        []
        []
    , input
        [ placeholder model.text
        , type' "text"
          -- on "input" targetValue (Signal.message address) - Why isn't this working?
          -- How to pass "AddTextToModel" the current value of the input?
        , on "input" targetValue (\str -> Signal.message address (ReverseString str))
        , style [ ( "display", "block" ) ]
        ]
        []
    , div
        []
        [ Html.text ("Outcome: " ++ model.text) ]
    , hr [] []
    , div
        []
        [ Html.text ("(Minute)-Progressbar: ") ]
    , div
        []
        [ Html.progress
            [ value (toString model.secs)
            , Html.Attributes.min "0"
            , Html.Attributes.max "60"
            ]
            []
        ]
    ]
