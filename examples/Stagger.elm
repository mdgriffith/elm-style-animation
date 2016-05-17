module Main exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, second)
import Style
import Style.Properties exposing (..)
import Style.Spring.Presets
import AnimationFrame


type alias Model =
    { widgets : List Widget
    , open : Bool
    }


type alias Widget =
    { style : Style.Animation
    }


type Msg
    = Show
    | Hide
    | Toggle
    | Animate Time


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Toggle ->
            if model.open then
                update Hide model
            else
                update Show model

        Show ->
            ( { model
                | open = True
                , widgets =
                    List.indexedMap
                        (\i widget ->
                            { widget
                                | style =
                                    Style.animate
                                        |> Style.delay (toFloat i * 0.05 * second)
                                        |> Style.spring Style.Spring.Presets.wobbly
                                        |> Style.to [ Left 100 Px ]
                                        |> Style.on widget.style
                            }
                        )
                        model.widgets
              }
            , Cmd.none
            )

        Hide ->
            ( { model
                | open = False
                , widgets =
                    List.indexedMap
                        (\i widget ->
                            { widget
                                | style =
                                    Style.animate
                                        |> Style.delay (toFloat i * 0.05 * second)
                                        |> Style.spring Style.Spring.Presets.wobbly
                                        |> Style.to [ Left -70 Px ]
                                        |> Style.on widget.style
                            }
                        )
                        model.widgets
              }
            , Cmd.none
            )

        Animate time ->
            ( { model
                | widgets =
                    List.map
                        (\widget ->
                            { widget
                                | style = Style.tick time widget.style
                            }
                        )
                        model.widgets
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        triggerStyle =
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
    in
        div
            [ onClick Toggle
            , style triggerStyle
            ]
            <| List.concat
                [ [ h1 [ style [ ( "padding", "25px" ) ] ]
                        [ text "Click me!" ]
                  , p [] [ text "This example shows staggered animations" ]
                  ]
                , List.map viewWidget model.widgets
                ]


viewWidget : Widget -> Html Msg
viewWidget model =
    let
        menuStyle =
            [ ( "border-radius", "20px" )
            , ( "width", "40px" )
            , ( "height", "40px" )
            , ( "position", "fixed" )
            , ( "background-color", "#4e9a06" )
            , ( "top", "0px" )
            , ( "z-index", "0" )
            , ( "display", "inline-block" )
            , ( "margin", "10px" )
            , ( "text-align", "center" )
            , ( "line-height", "40px" )
            ]
    in
        div [ style (menuStyle ++ (Style.render model.style)) ]
            []


init : ( Model, Cmd Msg )
init =
    ( { widgets = List.map (\i -> initWidget i) [0..10]
      , open = False
      }
    , Cmd.none
    )


initWidget i =
    { style =
        Style.init
            [ Left -50.0 Px
            , Top (i * 45.0) Px
            ]
    }


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
