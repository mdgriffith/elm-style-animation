import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Animation as UI
import Html.Animation.Properties exposing (..)
import Time exposing (Time, second)
import AnimationFrame

type alias Model = 
            { style : UI.Animation 
            }

-- UPDATE

type Action = ChangeColor
            | Animate Time

update : Action -> Model -> ( Model, Cmd Action )
update action model =
  case action of
    ChangeColor ->
      let
        style = 
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
            |> UI.on model.style
        in
          ( {model | style = style}
          , Cmd.none
          )

    Animate time ->
     ( { model 
            | style = UI.tick time model.style 
        }
    , Cmd.none)



view : Model -> Html Action
view model =
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
    div [ onClick ChangeColor
        , style (triggerStyle ++ UI.render model.style)
        ]

        [ text "Click to Change Color" ]


init : ( Model, Cmd Action )
init = ( { style = 
              UI.init 
                  [ BackgroundColor 
                        |> UI.rgba 58 40 69 1.0 ]
         }
       , Cmd.none )

subscriptions : Model -> Sub Action
subscriptions model =
  AnimationFrame.times Animate

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
