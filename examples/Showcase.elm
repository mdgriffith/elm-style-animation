module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Animation exposing (px, turn, percent)
import Color exposing (rgb, rgba)


type alias Model =
    { widgets : List Widget }


type alias Widget =
    { label : String
    , action : Msg
    , style : Animation.State
    }


type Msg
    = RotateWidget Int
    | RotateAllAxis Int
    | ChangeColors Int
    | ChangeMultipleColors Int
    | FadeOutFadeIn Int
    | Shadow Int
    | Animate Animation.Msg


onStyle : (Animation.State -> Animation.State) -> Widget -> Widget
onStyle styleFn widget =
    { widget | style = styleFn widget.style }


onIndex : Int -> List a -> (a -> a) -> List a
onIndex i list fn =
    List.indexedMap
        (\j val ->
            if i == j then
                fn val
            else
                val
        )
        list


onWidgetStyle : Model -> Int -> (Animation.State -> Animation.State) -> Model
onWidgetStyle model index fn =
    { model
        | widgets =
            onIndex index model.widgets <|
                onStyle fn
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        RotateWidget i ->
            ( onWidgetStyle model i <|
                (Animation.interrupt
                    [ Animation.to
                        [ Animation.rotate (turn 1) ]
                    , Animation.set
                        [ Animation.rotate (turn 0) ]
                    ]
                )
            , Cmd.none
            )

        RotateAllAxis i ->
            ( onWidgetStyle model i <|
                (Animation.interrupt
                    [ Animation.to
                        [ Animation.rotate3d (turn 1) (turn 1) (turn 1)
                        ]
                    , Animation.set
                        [ Animation.rotate3d (turn 0) (turn 0) (turn 0)
                        ]
                    ]
                )
            , Cmd.none
            )

        ChangeColors i ->
            ( onWidgetStyle model i <|
                (Animation.interrupt
                    [ Animation.to
                        [ Animation.backgroundColor (rgba 100 100 100 1.0)
                        , Animation.borderColor (rgba 100 100 100 1.0)
                        ]
                    , Animation.to
                        [ Animation.backgroundColor Color.white
                        , Animation.borderColor Color.white
                        ]
                    ]
                )
            , Cmd.none
            )

        ChangeMultipleColors i ->
            ( onWidgetStyle model i <|
                (Animation.interrupt <|
                    List.map
                        (\color ->
                            Animation.to
                                [ Animation.backgroundColor color
                                , Animation.borderColor color
                                ]
                        )
                        [ Color.red
                        , Color.orange
                        , Color.yellow
                        , Color.green
                        , Color.blue
                        , Color.purple
                        , Color.white
                        ]
                )
            , Cmd.none
            )

        FadeOutFadeIn i ->
            ( onWidgetStyle model i <|
                (Animation.interrupt
                    [ Animation.to
                        [ Animation.opacity 0
                        ]
                    , Animation.to
                        [ Animation.opacity 1
                        ]
                    ]
                )
            , Cmd.none
            )

        Shadow i ->
            ( onWidgetStyle model i <|
                (Animation.interrupt
                    [ Animation.to
                        [ Animation.translate (px 100) (px 100)
                        , Animation.scale 1.2
                        , Animation.shadow
                            { offsetX = 50
                            , offsetY = 55
                            , blur = 15
                            , size = 0
                            , color = rgba 0 0 0 0.1
                            }
                        ]
                    , Animation.to
                        [ Animation.translate (px 0) (px 0)
                        , Animation.scale 1
                        , Animation.shadow
                            { offsetX = 0
                            , offsetY = 1
                            , size = 0
                            , blur = 2
                            , color = rgba 0 0 0 0.1
                            }
                        ]
                    ]
                )
            , Cmd.none
            )

        Animate time ->
            ( { model
                | widgets =
                    List.map
                        (onStyle (Animation.update time))
                        model.widgets
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "left", "0px" )
            , ( "top", "0px" )
            , ( "width", "100%" )
            , ( "height", "100%" )
            , ( "background-color", "#f0f0f0" )
            ]
        ]
        [ div
            [ style
                [ ( "display", "flex" )
                , ( "flex-direction", "row" )
                , ( "flex-wrap", "wrap" )
                , ( "justify-content", "center" )
                , ( "position", "absolute" )
                , ( "left", "0px" )
                , ( "top", "0px" )
                , ( "width", "100%" )
                ]
            ]
            (List.map viewWidget model.widgets)
        ]


viewWidget : Widget -> Html Msg
viewWidget widget =
    div
        (Animation.render widget.style
            ++ [ style
                    [ ( "position", "relative" )
                    , ( "text-align", "center" )
                    , ( "cursor", "pointer" )
                    , ( "border-style", "solid" )
                    , ( "vertical-align", "middle" )
                    ]
               , onMouseOver (widget.action)
               ]
        )
        [ text widget.label ]


init : ( Model, Cmd Msg )
init =
    let
        initialWidgetStyle =
            Animation.style
                [ Animation.display Animation.inlineBlock
                , Animation.width (px 100)
                , Animation.height (px 100)
                , Animation.margin (px 50)
                , Animation.padding (px 25)
                , Animation.rotate (turn 0.0)
                , Animation.rotate3d (turn 0.0) (turn 0.0) (turn 0.0)
                , Animation.translate (px 0) (px 0)
                , Animation.opacity 1
                , Animation.backgroundColor Color.white
                , Animation.color Color.black
                , Animation.scale 1.0
                , Animation.borderColor Color.white
                , Animation.borderWidth (px 4)
                , Animation.borderRadius (px 8)
                , Animation.translate3d (percent 0) (percent 0) (px 0)
                , Animation.shadow
                    { offsetX = 0
                    , offsetY = 1
                    , size = 0
                    , blur = 2
                    , color = rgba 0 0 0 0.1
                    }
                ]
    in
        ( { widgets =
                [ { label = "Rotate"
                  , action = RotateWidget 0
                  , style = initialWidgetStyle
                  }
                , { label = "Rotate in All Kinds of Ways"
                  , action = RotateAllAxis 1
                  , style = initialWidgetStyle
                  }
                , { label = "Change Colors"
                  , action = ChangeColors 2
                  , style = initialWidgetStyle
                  }
                , { label = "Change Through Multiple Colors"
                  , action = ChangeMultipleColors 3
                  , style = initialWidgetStyle
                  }
                , { label = "Fade Out Fade In"
                  , action = FadeOutFadeIn 4
                  , style = initialWidgetStyle
                  }
                , { label = "Have a Shadow"
                  , action = Shadow 5
                  , style = initialWidgetStyle
                  }
                ]
          }
        , Cmd.none
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate <|
        List.map .style model.widgets


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
