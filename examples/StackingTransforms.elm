

import StartApp exposing (start)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects exposing (Effects, Never)
import Signal exposing (Address)
import Task

import Time exposing (second)

import Html.Animation as UI
import Html.Animation.Properties exposing (..)

import String exposing (concat)

type alias Model = 
            { style : UI.Animation 
            }

-- UPDATE

type Action = Transform
            | Animate UI.Action


{-| Prepare a helper function manage effects and assign styles

-}
onModel =
  UI.forwardTo 
      Animate
      .style
      (\w style -> { w | style = style }) -- style setter 


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
 
    Transform ->
      UI.animate 
            |> UI.duration (0.5*second)
            |> UI.props 
                [ Rotate (UI.to 20) Deg
                ] 
        |> UI.andThen
            |> UI.duration (0.7*second)
            |> UI.props 
                [ TranslateY (UI.to -200) Px
                ] 
        |> UI.andThen
            |> UI.duration (0.7*second)
            |> UI.props 
                [  Rotate UI.stay Deg  -- <-  Here's the only new function! 
                                            --  UI.stay allows us to specify 
                                            --  the 2nd Rotate we mentioned in our init
                , Rotate (UI.to 360) Deg
                ] 
          |> UI.andThen
            |> UI.duration (0.7*second)
            |> UI.props 
                [ Rotate (UI.to 380) Deg 
                ] 
        |> UI.andThen
            |> UI.delay (1*second)
            |> UI.props 
                [ Rotate (UI.to 0.0) Deg
                , TranslateY (UI.to 0.0) Px
                , Rotate (UI.to 0.0) Deg
                ] 
        |> onModel model

    Animate action ->
      onModel model action

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
              renderToString style = 
                  String.concat
                      <| List.map 
                          (\(name, value) -> name ++ ": " ++ value)
                          style
            in
              div [ onClick address Transform ]

                  [ div [ style <| boxStyle ++ (UI.render model.style) ]
                        [ h1 [ style [("padding","25px")]] 
                             [ text "Click to see a Stacked Transform"]
                        ]
                  , small [ style [ ("position", "fixed")
                                  , ("left","50px")
                                  , ("top", "50px")
                                  ]
                          ] 
                          [ text <| renderToString <| (UI.render model.style) ]
                  ]


init : ( Model, Effects Action )
init = ( { style = UI.init 
                        [ Rotate 0.0 Deg
                        , TranslateY 0.0 Px
                        , TranslateX 0.0 Px
                        , Rotate 0.0 Deg
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