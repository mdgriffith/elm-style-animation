

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

type alias Model = 
            { style : UI.Animation 
            }

-- UPDATE

type Action = ChangeColor
            | Animate UI.Action


{-| Prepare a helper function manage effects and assign styles -}
onModel =
  UI.forwardTo 
      Animate
      .style
      (\w style -> { w | style = style }) -- style setter 


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    ChangeColor ->
      UI.animate 
            |> UI.props 
                [ BackgroundColor 
                      |> UI.toRGBA 100 100 100 1.0  
                ] 
        |> UI.andThen -- create a new keyframe
            |> UI.duration (1*second)
            |> UI.props 
                [ BackgroundColor 
                      |> UI.toRGBA 178 201 14 1.0 
                ] 
        |> UI.andThen 
            |> UI.props 
                [ BackgroundColor 
                      |> UI.toRGBA 58 40 69 1.0 
                ] 
        |> onModel model

    Animate action ->
      onModel model action


view : Address Action -> Model -> Html
view address model =
            let
              triggerStyle = [ ("position", "relative")
                             , ("margin", "200px auto")
                             , ("width", "250px")
                             , ("height", "250px")
                             , ("text-align","center")
                             , ("line-height", "250px")
                             , ("color", "white")
                             , ("cursor", "pointer")
                             ]
            in
              div [ onClick address ChangeColor
                  , style (triggerStyle ++ UI.render model.style)
                  ]

                  [ text "Click to Change Color" ]


init : ( Model, Effects Action )
init = ( { style = 
              UI.init 
                  [ BackgroundColor 
                        |> UI.rgba 58 40 69 1.0 ]
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