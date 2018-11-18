module Main exposing (..)

import Animation exposing (px)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { style : Animation.State
    }


type Msg
    = Show
    | Hide
    | Animate Animation.Msg


type alias Styles =
    { open : List Animation.Property
    , closed : List Animation.Property
    }


styles : Styles
styles =
    { open =
        [ Animation.left (px 0.0)
        , Animation.opacity 1.0
        ]
    , closed =
        [ Animation.left (px -200.0)
        , Animation.opacity 0
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Show ->
            ( { model
                | style =
                    Animation.interrupt
                        [ Animation.to styles.open
                        ]
                        model.style
              }
            , Cmd.none
            )

        Hide ->
            ( { model
                | style =
                    Animation.interrupt
                        [ Animation.to styles.closed
                        ]
                        model.style
              }
            , Cmd.none
            )

        Animate animMsg ->
            ( { model
                | style = Animation.update animMsg model.style
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div
        [ onMouseEnter Show
        , onMouseLeave Hide
        , style "position" "absolute"
        , style "left" "0px"
        , style "top" "0px"
        , style "width" "350px"
        , style "height" "100%"
        , style "border" "2px dashed #AAA"
        ]
        [ h1 [ style "padding" "25px" ]
            [ text "Hover here to see menu!" ]
        , div
            (Animation.render model.style
                ++ [ style "position" "absolute"
                   , style "top" "-2px"
                   , style "margin-left" "-2px"
                   , style "padding" "25px"
                   , style "width" "300px"
                   , style "height" "100%"
                   , style "background-color" "rgb(58,40,69)"
                   , style "color" "white"
                   , style "border" "2px solid rgb(58,40,69)"
                   ]
            )
            [ h1 [] [ text "Hidden Menu" ]
            , ul []
                [ li [] [ text "Some things" ]
                , li [] [ text "in a list" ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate [ model.style ]


init : ( Model, Cmd Msg )
init =
    ( { style = Animation.style styles.closed }
    , Cmd.none
    )


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
