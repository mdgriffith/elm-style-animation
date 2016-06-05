module Main exposing (..) --where

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import AnimationFrame
import Time exposing (second)
import Style
import Style.Properties exposing (..)
import Color exposing (green, complement)
import Ease

type alias Model =
    { style : Style.Animation
    }


type Msg
    = AnimateRotate
    | Animate Float


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        AnimateRotate ->
            ( { model
                | style =
                    Style.animate
                        --|> Style.duration (1*second)
                        --|> Style.easing Ease.inOutQuad
                        |> Style.spring { stiffness = 250
                                        , damping = 15
                                        }
                        |> Style.to
                                 [ Rotate 360 Deg ]
                        |> Style.on model.style
              }
            , Cmd.none
            )

        Animate time ->
            ( { model
                | style = Style.tick time model.style
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div
        [ onClick AnimateRotate
        , class "dotted-box"
        ]
        [ div
            [ style (Style.render model.style)
            , class "box"
            ] []
        , div [class "text"] [text "Elm + Elm Style Animation"]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.times Animate


init : ( Model, Cmd Msg )
init =
    ( { style = Style.init [ Rotate 0 Deg ] }
    , Cmd.none
    )


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
