module Main exposing (..) 

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Style
import Style.Properties exposing (..)
import Time exposing (Time, second)
import AnimationFrame
import Color exposing (rgba)


type alias Model =
    { style : Style.Animation
    }


type Msg
    = ChangeColor
    | Animate Time


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        ChangeColor ->
            let
                style =
                    Style.animate
                        |>
                            Style.to
                                [ BackgroundColor (rgba 100 100 100 1.0)
                                ]
                        |>
                            Style.andThen -- create a new keyframe
                        |>
                            Style.duration (1 * second)
                        |>
                            Style.to
                                [ BackgroundColor (rgba 178 201 14 1.0)
                                ]
                        |>
                            Style.andThen
                        |>
                            Style.to
                                [ BackgroundColor (rgba 58 40 69 1.0)
                                ]
                        |>
                            Style.on model.style
            in
                ( { model | style = style }
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
    let
        triggerStyle =
            [ ( "position", "relative" )
            , ( "margin", "200px auto" )
            , ( "width", "250px" )
            , ( "height", "250px" )
            , ( "text-align", "center" )
            , ( "line-height", "250px" )
            , ( "color", "white" )
            , ( "cursor", "pointer" )
            ]
    in
        div
            [ onClick ChangeColor
            , style (triggerStyle ++ Style.render model.style)
            ]
            [ text "Click to Change Color" ]


init : ( Model, Cmd Msg )
init =
    ( { style =
            Style.init [ BackgroundColor (rgba 58 40 69 1.0) ]
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.times Animate


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
