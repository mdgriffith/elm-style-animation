module Main exposing (..) --where

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Style
import Style.Properties exposing (..)
import AnimationFrame
import Time exposing (Time, second)
import String exposing (concat)


type alias Model =
    { style : Style.Animation
    }


type Msg
    = Transform
    | Animate Time


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Transform ->
            let
                style =
                    Style.animate
                        |> Style.duration (0.5 * second)
                        |> Style.to
                            [ Rotate 20 Deg
                            ]
                        |> Style.andThen
                        |> Style.duration (0.7 * second)
                        |> Style.to
                            [ TranslateY -200 Px
                            ]
                        |> Style.andThen
                        |> Style.duration (0.7 * second)
                        |> Style.update
                            [ Rotate identity Deg
                            , Rotate (\_ -> 360) Deg
                            ]
                        |> Style.andThen
                        |> Style.duration (0.7 * second)
                        |> Style.to
                            [ Rotate 380 Deg
                            ]
                        |> Style.andThen
                        |> Style.delay (1 * second)
                        |> Style.to
                            [ Rotate 0.0 Deg
                            , TranslateY 0.0 Px
                            , Rotate 0.0 Deg
                            ]
                        |> Style.on model.style
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
        boxStyle =
            [ ( "position", "relative" )
            , ( "left", "0px" )
            , ( "top", "0px" )
            , ( "width", "300px" )
            , ( "margin-top", "250px" )
            , ( "margin-left", "auto" )
            , ( "margin-right", "auto" )
            , ( "padding", "25px" )
            , ( "text-align", "center" )
            , ( "border-radius", "5px" )
            , ( "background-color", "#AAA" )
            , ( "cursor", "pointer" )
            ]

        renderToString style =
            String.concat
                <| List.map (\( name, value ) -> name ++ ": " ++ value)
                    style
    in
        div [ onClick Transform ]
            [ div [ style <| boxStyle ++ (Style.render model.style) ]
                [ h1 [ style [ ( "padding", "25px" ) ] ]
                    [ text "Click to see a Stacked Transform" ]
                ]
            , small
                [ style
                    [ ( "position", "fixed" )
                    , ( "left", "50px" )
                    , ( "top", "50px" )
                    ]
                ]
                [ text <| renderToString <| (Style.render model.style) ]
            ]


init : ( Model, Cmd Msg )
init =
    ( { style =
            Style.init
                [ Rotate 0.0 Deg
                , TranslateY 0.0 Px
                , TranslateX 0.0 Px
                , Rotate 0.0 Deg
                ]
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
