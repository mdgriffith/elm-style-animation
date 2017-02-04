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
        [ describe "Value Expectations"
            [ test "Opacity 0 -> 1" <|
                \() ->
                    animateOpacity
                        |> fastforward (600 * millisecond)
                        |> Animation.Render.renderValues
                        |> Tuple.first
                        |> Expect.equal [ "opacity" => "1" ]
            , test "Two Consecutive Animations" <|
                \() ->
                    animateOpacityTwoStep
                        |> fastforward (1300 * millisecond)
                        |> Animation.Render.renderValues
                        |> Tuple.first
                        |> Expect.equal [ "opacity" => "1" ]
            ]
        ]


animateOpacityTwoStep =
    Animation.interrupt
        [ Animation.toWith
            (Animation.easing
                { duration = 600
                , ease = identity
                }
            )
            [ Animation.opacity 0.3 ]
        , Animation.toWith
            (Animation.easing
                { duration = 600
                , ease = identity
                }
            )
            [ Animation.opacity 1.0 ]
        ]
        (Animation.style [ Animation.opacity 1.0 ])


animateOpacity =
    Animation.interrupt
        [ Animation.toWith
            (Animation.easing
                { duration = 600
                , ease = identity
                }
            )
            [ Animation.opacity 1.0 ]
        ]
        (Animation.style [ Animation.opacity 0.0 ])


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
