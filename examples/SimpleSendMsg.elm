module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Animation exposing (px)
import Animation.Messenger


main : Program Never Model Msg
main =
    Html.program
        { init = init
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
               , style
                    [ ( "position", "relative" )
                    , ( "margin", "100px auto" )
                    , ( "padding", "25px" )
                    , ( "width", "200px" )
                    , ( "height", "200px" )
                    , ( "cursor", "pointer" )
                    , ( "background-color", "#268bd2" )
                    , ( "color", "white" )
                    ]
               ]
        )
        [ text "Click to Animate! (Check console for sent message)" ]
