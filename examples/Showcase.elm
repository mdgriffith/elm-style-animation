module Main exposing (..)

--where

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy
import Style
import Style.Properties exposing (..)
import Style.Spring.Presets
import AnimationFrame
import Color exposing (rgb, rgba)
import Time exposing (Time, second)
import Ease


type alias Model =
    { widgets : List Widget }


type alias Widget =
    { label : String
    , style : Style.Animation
    , action : Int -> Msg
    }


type Msg
    = RotateWidget Int
    | RotateAllAxis Int
    | RotateCustomEasingDuration Int
    | ChangeColors Int
    | ChangeMultipleColors Int
    | FadeOutFadeIn Int
    | FadeOut Int
    | Loopty Int
    | Spring Int
    | Animate Time


mapToIndex : Int -> (a -> a) -> List a -> List a
mapToIndex j fn list =
    List.indexedMap
        (\i x ->
            if i == j then
                fn x
            else
                x
        )
        list


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        RotateWidget i ->
            let
                widgets =
                    -- Interrupt any animation on this element and start this animation
                    Style.animate
                        |> Style.update
                            [ Rotate ((+) 1.0) Turn
                            ]
                        |> (\act ->
                                mapToIndex i
                                    (\widget ->
                                        { widget
                                            | style = Style.on widget.style act
                                        }
                                    )
                                    model.widgets
                           )
            in
                ( { model | widgets = widgets }
                , Cmd.none
                )

        RotateAllAxis i ->
            let
                widgets =
                    -- queue up this animation
                    -- as opposed to interrupting
                    Style.queue
                        |> Style.update
                            [ RotateX ((+) 1) Turn
                            , RotateY ((+) 1) Turn
                            , Rotate ((+) 1) Turn
                            ]
                        |> (\act ->
                                mapToIndex i
                                    (\widget ->
                                        { widget
                                            | style = Style.on widget.style act
                                        }
                                    )
                                    model.widgets
                           )
            in
                ( { model | widgets = widgets }
                , Cmd.none
                )

        RotateCustomEasingDuration i ->
            let
                widgets =
                    Style.queue
                        |> Style.duration (2 * second)
                        |> Style.easing Ease.inBounce
                        |> Style.update
                            [ Rotate ((+) 1) Turn
                            ]
                        |> (\act ->
                                mapToIndex i
                                    (\widget ->
                                        { widget
                                            | style = Style.on widget.style act
                                        }
                                    )
                                    model.widgets
                           )
            in
                ( { model | widgets = widgets }
                , Cmd.none
                )

        Loopty i ->
            let
                widgets =
                    Style.queue
                        |> Style.easing Ease.linear
                        |> Style.duration (0.4 * second)
                        |> Style.update
                            [ Rotate (\x -> x - 0.5) Turn
                            , Rotate ((+) 0.5) Turn
                            , TranslateY (\_ -> 50) Px
                            ]
                        |> Style.andThen
                        |> Style.easing Ease.linear
                        |> Style.duration (0.4 * second)
                        |> Style.update
                            [ Rotate (\x -> x - 0.5) Turn
                            , Rotate ((+) 0.5) Turn
                            , TranslateY (\_ -> 0) Px
                            ]
                        |> (\act ->
                                mapToIndex i
                                    (\widget ->
                                        { widget
                                            | style = Style.on widget.style act
                                        }
                                    )
                                    model.widgets
                           )
            in
                ( { model | widgets = widgets }
                , Cmd.none
                )

        Spring i ->
            let
                widgets =
                    Style.queue
                        |> Style.spring Style.Spring.Presets.noWobble
                        |> Style.to
                            [ Scale 1.5
                            ]
                        |> Style.andThen
                        |> Style.spring Style.Spring.Presets.wobbly
                        |> Style.to
                            [ Scale 1.0
                            ]
                        |> (\act ->
                                mapToIndex i
                                    (\widget ->
                                        { widget
                                            | style = Style.on widget.style act
                                        }
                                    )
                                    model.widgets
                           )
            in
                ( { model | widgets = widgets }
                , Cmd.none
                )

        ChangeColors i ->
            let
                widgets =
                    Style.animate
                        |> Style.to
                            [ BackgroundColor (rgba 100 100 100 1.0)
                            , BorderColor (rgba 100 100 100 1.0)
                            ]
                        |> (\act ->
                                mapToIndex i
                                    (\widget ->
                                        { widget
                                            | style = Style.on widget.style act
                                        }
                                    )
                                    model.widgets
                           )
            in
                ( { model | widgets = widgets }
                , Cmd.none
                )

        ChangeMultipleColors i ->
            let
                widgets =
                    -- animate is used to interrupt whatever current animation
                    -- is running and smoothely move to the new style
                    Style.animate
                        |> Style.to
                            [ BackgroundColor (rgba 100 100 100 1.0)
                            , BorderColor (rgba 100 100 100 1.0)
                            ]
                        |> Style.andThen
                        |> Style.to
                            [ BackgroundColor (rgba 178 201 14 1.0)
                            , BorderColor (rgba 178 201 14 1.0)
                            ]
                        |> (\act ->
                                mapToIndex i
                                    (\widget ->
                                        { widget
                                            | style = Style.on widget.style act
                                        }
                                    )
                                    model.widgets
                           )
            in
                ( { model | widgets = widgets }
                , Cmd.none
                )

        FadeOutFadeIn i ->
            let
                widgets =
                    Style.animate
                        |> Style.to [ Opacity 0 ]
                        |> Style.andThen
                        |> Style.to [ Opacity 1 ]
                        |> (\act ->
                                mapToIndex i
                                    (\widget ->
                                        { widget
                                            | style = Style.on widget.style act
                                        }
                                    )
                                    model.widgets
                           )
            in
                ( { model | widgets = widgets }
                , Cmd.none
                )

        FadeOut i ->
            let
                widgets =
                    Style.animate
                        |> Style.to
                            [ Opacity 0
                            ]
                        |> Style.andThen
                        |> Style.set
                            [ Display None
                            ]
                        |> (\act ->
                                mapToIndex i
                                    (\widget ->
                                        { widget
                                            | style = Style.on widget.style act
                                        }
                                    )
                                    model.widgets
                           )
            in
                ( { model | widgets = widgets }
                , Cmd.none
                )

        Animate time ->
            ( { model
                | widgets =
                    List.map
                        (\widget ->
                            { widget | style = Style.tick time widget.style }
                        )
                        model.widgets
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    let
        triggerStyle =
            [ ( "position", "relative" )
            , ( "left", "0px" )
            , ( "top", "0px" )
            , ( "width", "100%" )
            , ( "height", "100%" )
            ]
    in
        div [ style triggerStyle ]
            <| List.indexedMap (\i w -> Html.Lazy.lazy2 box i w)
                model.widgets


box : Int -> Widget -> Html Msg
box i widget =
    let
        boxStyle =
            [ ( "position", "relative" )
            , ( "display", "inline-block" )
            , ( "margin", "50px 50px" )
            , ( "padding", "25px" )
            , ( "text-align", "center" )
            , ( "width", "100px" )
            , ( "height", "100px" )
            , ( "color", "white" )
            , ( "cursor", "pointer" )
            , ( "border-style", "solid" )
            , ( "vertical-align", "middle" )
            ]
    in
        div
            [ style (boxStyle ++ Style.render widget.style)
            , onClick (widget.action i)
            ]
            [ text widget.label ]


initialWidgetStyle =
    Style.init
        [ Display InlineBlock
        , Rotate 0.0 Turn
        , RotateX 0.0 Turn
        , RotateY 0.0 Turn
        , TranslateY 0.0 Px
        , TranslateX 0.0 Px
        , Rotate 0.0 Turn
        , Opacity 1
        , BackgroundColor (rgba 58 40 69 1.0)
        , Color (rgba 255 255 255 1.0)
        , Scale 1.0
        , BorderColor (rgb 136 96 161)
        , BorderWidth 4 Px
        , BorderRadius 8 Px
        ]


init : ( Model, Cmd Msg )
init =
    ( { widgets =
            [ { label = "Rotate"
              , style = initialWidgetStyle
              , action = RotateWidget
              }
            , { label = "Rotate in All Kinds of Ways"
              , style = initialWidgetStyle
              , action = RotateAllAxis
              }
            , { label = "Rotate with custom easing and duration"
              , style = initialWidgetStyle
              , action = RotateCustomEasingDuration
              }
            , { label = "Change Colors"
              , style = initialWidgetStyle
              , action = ChangeColors
              }
            , { label = "Change Through Multiple Colors"
              , style = initialWidgetStyle
              , action = ChangeMultipleColors
              }
            , { label = "Fade Out Fade In"
              , style = initialWidgetStyle
              , action = FadeOutFadeIn
              }
            , { label = "Fade Out and display:none"
              , style = initialWidgetStyle
              , action = FadeOut
              }
            , { label = "Loop About"
              , style = initialWidgetStyle
              , action = Loopty
              }
            , { label = "Use a Spring"
              , style = initialWidgetStyle
              , action = Spring
              }
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
