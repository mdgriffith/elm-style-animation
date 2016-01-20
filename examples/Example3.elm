

module ElmUIExampleTwo where

import StartApp exposing (start)

import Effects exposing (Effects, Never)
import Task
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Time exposing (Time, second)
import Signal exposing (Address)

import ElmUI as UI
import Animation exposing (..)

import Debug

import Multi

-- MODEL

type alias Model = { animations : Multi.Model }


-- UPDATE

type Action = Rotate Int
            | Blink
            --| Tilt
            | Animate Multi.Action



update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Rotate i ->
      let 
        (anim, fx) = Multi.animate i 
                            (UI.start 
                                [ UI.Rotate UI.Turn (UI.add 1) ]
                            )
                            model.animations
      in
        ( { model | animations = anim }
        , Effects.map Animate fx )

    Blink ->
      let 
        (anim, fx) = Multi.animate 2 
                            (UI.start 
                                [ UI.Scale (UI.to 2) ]
                            )
                            model.animations
      in
        ( { model | animations = anim }
        , Effects.map Animate fx )


    Animate action ->
      let
        (anim, fx) = Multi.update action model.animations
      in
        ( { model | animations = anim }
        , Effects.map Animate fx )




-- VIEW


view : Address Action -> Model -> Html
view address model =
            let
              triggerStyle = [ ("position", "relative")
                             , ("left", "0px")
                             , ("top", "0px")
                             , ("width", "100%")
                             , ("height", "100%")
                            ]
            in
              div [ style triggerStyle ]

                  [ box address model (Rotate 1) 1
                  , box address model Blink 2
                  , box address model (Rotate 3) 3
                  ]


box : Address Action -> Model -> Action -> Int -> Html
box address model action animI = 
               let
                  boxStyle = [ ("position", "relative")
                              , ("display", "inline-block")
                                , ("margin-left", "20px")
                                , ("margin-top", "50px")
                                , ("padding", "25px")
                                , ("text-align", "center")
                                , ("width", "100px")
                                , ("height", "100px")
                                , ("background-color", "rgb(58,40,69)")
                                , ("color", "white")
                                , ("cursor", "pointer")
                              ]
                in
                  div [ style (boxStyle ++ (Multi.render animI model.animations))
                      , onClick address action ]
                      [ text "Poke Me" ]




init : ( Model, Effects Action )
init = ( { animations = [
            (1, UI.init [ UI.Rotate UI.Turn 0.0 ] )
          , (2, UI.init [ UI.Scale 1.0 ])
          , (3, UI.init [ UI.Rotate UI.Turn 0.0 ] )
          ]
        }
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