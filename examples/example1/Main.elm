

module ElmUIExampleOne where

import StartApp exposing (start)

import Effects exposing (Effects, Never)
import Task
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Time exposing (Time, second)
import Signal exposing (Address)

import ElmUI as UI

import Debug


-- MODEL

type alias Model = { i : Int
                   , animation : UI.Model 
                   }


-- UPDATE

type Action = Increment 
            | Decrement
            | Animate UI.Action



update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Increment ->
      let 
        ( anim, fx ) = UI.update 
                            (UI.Begin 
                                  <| UI.FadeIn (2*second))
                            model.animation
      in
        ( { model | i = model.i + 1
                  , animation = anim }
        , Effects.map Animate fx )

    Decrement ->
        ( { model | i = model.i - 1 }
        , Effects.none )

    Animate action ->
      let
        (anim, fx) = UI.update action model.animation
      in
        ( { model | animation = anim }
        , Effects.map Animate fx )




-- VIEW

view : Address Action -> Model -> Html
view address model =
            div [ style (UI.render model.animation) ]
                [ button [ onClick address Decrement ] [ text "-" ]
                , div [ ] [ text (toString model.i) ]
                , button [ onClick address Increment ] [ text "+" ]
                ]

init : ( Model, Effects Action )
init = ( { i=0
         , animation=UI.empty }
       , Effects.none )

app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }

main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks