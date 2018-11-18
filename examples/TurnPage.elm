module Main exposing (..)

import Animation exposing (px)
import Animation.Messenger
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { style : Animation.Messenger.State Msg
    , pages : List String
    , current : Int
    }


type Msg
    = Next
    | Switch
    | Animate Animation.Msg


{-|

    Normally doing a function like this isn't good practice.
    But this example is to show animation!

-}
get : Int -> List String -> String
get i list =
    list
        |> List.drop i
        |> List.head
        |> Maybe.withDefault "Unknown Page"


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Next ->
            ( { model
                | style =
                    Animation.interrupt
                        [ Animation.to
                            [ Animation.opacity 0.0
                            ]
                        , Animation.Messenger.send Switch
                        , Animation.to
                            [ Animation.opacity 1.0
                            ]
                        ]
                        model.style
              }
            , Cmd.none
            )

        Switch ->
            ( { model
                | current =
                    if model.current + 1 >= List.length model.pages then
                        0
                    else
                        model.current + 1
              }
            , Cmd.none
            )

        Animate animMsg ->
            let
                ( anim, cmd ) =
                    Animation.Messenger.update animMsg model.style
            in
            ( { model
                | style = anim
              }
            , cmd
            )


view : Model -> Html Msg
view model =
    div
        [ onClick Next
        , style "position" "absolute"
        , style "top" "100px"
        , style "left" "50%"
        , style "margin-left" "-150px"
        , style "width" "300px"
        , style "cursor" "pointer"
        ]
        [ div
            (Animation.render model.style
                ++ [ style "position" "absolute"
                   , style "top" "-2px"
                   , style "padding" "25px"
                   , style "width" "300px"
                   , style "background-color" "#fafafa"
                   , style "border" "1px solid #eee"
                   , style "border-radius" "3px"
                   ]
            )
            [ h1 [] [ text (get model.current model.pages) ]
            , p [] [ text "Here's some other content!  Click to Switch!" ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate [ model.style ]


init : ( Model, Cmd Msg )
init =
    ( { style =
            Animation.style
                [ Animation.opacity 1.0
                ]
      , pages = [ "Page One", "Page Two", "Page Three" ]
      , current = 0
      }
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
