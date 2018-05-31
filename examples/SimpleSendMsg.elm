module Main exposing (..)

import Animation exposing (px)
import Animation.Messenger
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Browser.embed
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { style : Animation.Messenger.State Msg }


init : ( Model, Cmd Msg )
init =
    ( { style =
            Animation.style
                [ Animation.opacity 1.0
                ]
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate [ model.style ]


type Msg
    = FadeInFadeOut
    | Print String
    | Animate Animation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        FadeInFadeOut ->
            ( { model
                | style =
                    Animation.interrupt
                        [ Animation.to
                            [ Animation.opacity 0
                            ]
                        , Animation.Messenger.send (Print "Hi!")
                        , Animation.to
                            [ Animation.opacity 1
                            ]
                        ]
                        model.style
              }
            , Cmd.none
            )

        Print str ->
            let
                _ =
                    Debug.log "message sent" str
            in
            ( model, Cmd.none )

        Animate animMsg ->
            let
                ( newStyle, cmd ) =
                    Animation.Messenger.update animMsg model.style
            in
            ( { model
                | style = newStyle
              }
            , cmd
            )


view : Model -> Html Msg
view model =
    div
        (Animation.render model.style
            ++ [ onClick FadeInFadeOut
               , style "position" "relative"
               , style "margin" "100px auto"
               , style "padding" "25px"
               , style "width" "200px"
               , style "height" "200px"
               , style "cursor" "pointer"
               , style "background-color" "#268bd2"
               , style "color" "white"
               ]
        )
        [ text "Click to Animate! (Check console for sent message)" ]
