module Tests exposing (..)

import Test exposing (..)
import Expect
import Animation
import Animation.Model
import Animation.Render
import Time exposing (..)
import Ease


(=>) =
    (,)


all : Test
all =
    describe "Animation Test Suite"
        [ describe "Easing Based Animations"
            [ test "Opacity 0 -> 1" <|
                \() ->
                    easeOpacity
                        |> fastforward (500 * millisecond)
                        |> Animation.Render.renderValues
                        |> Tuple.first
                        |> Expect.equal [ "opacity" => "1" ]
            , test "Two Consecutive Animations" <|
                \() ->
                    -- Should take exactly 1200 milliseconds
                    easeOpacityTwoStep
                        |> fastforward (1000 * millisecond)
                        |> Animation.Render.renderValues
                        |> Tuple.first
                        |> Expect.equal [ "opacity" => "1" ]
            , test "Steps After Repeat" <|
                \() ->
                    toRepeatTo
                        |> fastforward (4128 * millisecond)
                        |> Animation.Render.renderValues
                        |> Tuple.first
                        |> Expect.equal [ "opacity" => "0" ]
            , test "Repeat Zero" <|
                \() ->
                    repeatZero
                        |> fastforward (500 * millisecond)
                        |> Animation.Render.renderValues
                        |> Tuple.first
                        |> Expect.equal [ "opacity" => "1" ]
            ]
        , describe "Spring Based Animation"
            [ test "Opacity 0 -> 1" <|
                \() ->
                    -- Standard spring takes ~ 560 milliseconds to finish.
                    springOpacity
                        |> fastforward (560 * millisecond)
                        |> Animation.Render.renderValues
                        |> Tuple.first
                        |> Expect.equal [ "opacity" => "1" ]
            , test "Two Consecutive Animations" <|
                \() ->
                    -- Why is this two step animation not 2* as long as the first?
                    springOpacityTwoStep
                        |> fastforward (1045 * millisecond)
                        |> Animation.Render.renderValues
                        |> Tuple.first
                        |> Expect.equal [ "opacity" => "1" ]
            ]
        ]


standardEasing =
    Animation.easing
        { duration = 500
        , ease = identity
        }


repeatZero =
    Animation.interrupt
        [ Animation.repeat 0
            [ Animation.to
                [ Animation.opacity 0.0 ]
            ]
        ]
        (Animation.styleWith standardEasing [ Animation.opacity 1.0 ])


toRepeatTo =
    Animation.interrupt
        [ Animation.repeat 3
            [ Animation.to
                [ Animation.opacity 1.0 ]
            , Animation.to
                [ Animation.opacity 0.5 ]
            ]
        , Animation.to
            [ Animation.opacity 0.0 ]
        ]
        (Animation.styleWith standardEasing [ Animation.opacity 1.0 ])


easeOpacityTwoStep =
    Animation.interrupt
        [ Animation.to
            [ Animation.opacity 0.3 ]
        , Animation.to
            [ Animation.opacity 1.0 ]
        ]
        (Animation.styleWith standardEasing [ Animation.opacity 1.0 ])


easeOpacity =
    Animation.interrupt
        [ Animation.to
            [ Animation.opacity 1.0 ]
        ]
        (Animation.styleWith standardEasing [ Animation.opacity 0.0 ])


springOpacity =
    Animation.interrupt
        [ Animation.to
            [ Animation.opacity 1.0 ]
        ]
        (Animation.style [ Animation.opacity 0.0 ])


springOpacityTwoStep =
    Animation.interrupt
        [ Animation.to
            [ Animation.opacity 0.3 ]
        , Animation.to
            [ Animation.opacity 1.0 ]
        ]
        (Animation.style [ Animation.opacity 1.0 ])


fastforward duration animation =
    let
        steps =
            spoofAnimationFrameTimes duration
                |> List.map Animation.Model.Tick
    in
        List.foldl Animation.update animation steps


spoofAnimationFrameTimes : Time -> List Time
spoofAnimationFrameTimes duration =
    let
        numberOfSteps =
            floor (inMilliseconds duration / (16 * millisecond))

        remaining =
            (inMilliseconds duration - (toFloat numberOfSteps * 16 * millisecond)) * millisecond

        steps =
            List.repeat numberOfSteps (16 * millisecond)

        asTimes deltas =
            List.reverse <|
                List.foldl
                    (\time previous ->
                        case previous of
                            [] ->
                                [ time ]

                            prev :: tail ->
                                prev + time :: prev :: tail
                    )
                    []
                    deltas
    in
        if remaining > 0 then
            asTimes <| steps ++ [ remaining ]
        else
            asTimes <| steps
