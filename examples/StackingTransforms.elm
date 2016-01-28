

import StartApp exposing (start)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects exposing (Effects, Never)
import Signal exposing (Address)
import Task

import Time exposing (second)

import Html.Animation as UI



type alias Model = 
            { style : UI.Animation 
            }

-- UPDATE

type Action = Transform
            | Animate UI.Action



update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
 
    Transform ->
      let 
        (anim, fx) = 
              UI.animate 
                  |> UI.duration (0.5*second)
                  |> UI.props 
                      [ UI.Rotate (UI.to 20) UI.Deg
                      ] 
              |> UI.andThen
                  |> UI.duration (0.7*second)
                  |> UI.props 
                      [ UI.TranslateY (UI.to -200) UI.Px
                      ] 
              |> UI.andThen
                  |> UI.duration (0.7*second)
                  |> UI.props 
                      [ UI.Rotate UI.stay UI.Deg  -- <-  Here's the only new function! 
                                                  --  UI.stay allows us to specify 
                                                  --  the 2nd Rotate we mentioned in our init
                      , UI.Rotate (UI.to 360) UI.Deg
                      ] 
                |> UI.andThen
                  |> UI.duration (0.7*second)
                  |> UI.props 
                      [ UI.Rotate (UI.to 380) UI.Deg 
                      ] 
              |> UI.andThen
                  |> UI.delay (1*second)
                  |> UI.props 
                      [ UI.Rotate (UI.to 0.0) UI.Deg
                      , UI.TranslateY (UI.to 0.0) UI.Px
                      , UI.Rotate (UI.to 0.0) UI.Deg
                      ] 
              |> UI.on model.style
      in
        ( { model | style = anim }
        , Effects.map Animate fx )


    Animate action ->
      let
        (anim, fx) = UI.update action model.style
      in
        ( { model | style = anim }
        , Effects.map Animate fx )



view : Address Action -> Model -> Html
view address model =
            let
              boxStyle = [ ("position", "relative")
                             , ("left", "0px")
                             , ("top", "0px")
                             , ("width", "300px")
                             , ("margin-top", "250px")
                             , ("margin-left", "auto")
                             , ("margin-right", "auto")
                             , ("padding", "25px")
                             , ("text-align", "center")
                             , ("border-radius", "5px")
                             , ("background-color", "#AAA")
                             , ("cursor", "pointer")
                            ]
            in
              div [ onClick address Transform
                  , style <| boxStyle ++ (UI.render model.style)
                  ]

                  [ h1 [ style [("padding","25px")]] 
                       [ text "Click to see a Stacked Transform"]
                  ]


init : ( Model, Effects Action )
init = ( { style = UI.init 
                        [ UI.Rotate 0.0 UI.Deg
                        , UI.TranslateY 0.0 UI.Px
                        , UI.TranslateX 0.0 UI.Px
                        , UI.Rotate 0.0 UI.Deg
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