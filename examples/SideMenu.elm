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

type alias Model =
    { style : Style.Animation
    }


type Msg
    = Show
    | Hide
    | Animate Float


styles =
    { open =
        [ Left 0.0 Px
        , Opacity 1.0
        , Color (green)
        ]
    , closed =
        [ Left -350.0 Px
        , Opacity 0.0
        , Color (green)
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Show ->
            ( { model
                | style =
                    Style.animate
                        |> Style.duration (5*second)
                        |> Style.easing (\x -> x)
                        |> Style.to styles.open
                        |> Style.on model.style
              }
            , Cmd.none
            )

        Hide ->
            ( { model
                | style =
                    Style.animate
                        |> Style.duration (5*second)
                        |> Style.easing (\x -> x)
                        |> Style.to styles.closed
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
        [ onMouseEnter Show
        , onMouseLeave Hide
        , style
            [ ( "position", "absolute" )
            , ( "left", "0px" )
            , ( "top", "0px" )
            , ( "width", "350px" )
            , ( "height", "100%" )
            , ( "border", "2px dashed #AAA" )
            ]
        ]
        [ h1 [ style [ ( "padding", "25px" ) ] ]
            [ text "Hover here to see menu!" ]
        , div
            [ style
                ([ ( "position", "absolute" )
                 , ( "top", "-2px" )
                 , ( "margin-left", "-2px" )
                 , ( "padding", "25px" )
                 , ( "width", "300px" )
                 , ( "height", "100%" )
                 , ( "background-color", "rgb(58,40,69)" )
                 , ( "color", "white" )
                 , ( "border", "2px solid rgb(58,40,69)" )
                 ]
                    ++ (Style.render model.style)
                )
            ]
            [ h1 [] [ text "Hidden Menu" ]
            , ul []
                [ li [] [ text "Some things" ]
                , li [] [ text "in a list" ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.times Animate


init : ( Model, Cmd Msg )
init =
    ( { style = Style.init styles.closed }
    , Cmd.none
    )


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
