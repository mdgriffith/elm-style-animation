module Style exposing (Animation, init, update, render, renderAttr, animate, queue, on, delay, duration, easing, spring, andThen, set, tick, to)

{-| This library is for animating css properties and is meant to work well with elm-html.

The easiest way to get started with this library is to check out the examples that are included with the [source code](https://github.com/mdgriffith/elm-style-animation).

Once you have the basic structure of how to use this library, you can refer to this documentation to fill any gaps.


# Base Definitions
@docs Animation

# Starting an animation
@docs animate, queue

# Creating animations
@docs delay, spring, duration, easing, andThen

# Animating Properties
@docs to, set, update

# Render a Animation into CSS or as SVG attributes
@docs render, renderAttr

# Set the starting style
@docs init

# Managing Commands
@docs on, tick

-}

import Time exposing (Time, second)
import String exposing (concat)
import List
import Color
import Style.PropertyHelpers exposing (Style, emptyEasing, Static)
import Style.Spring as Spring
import Style.Spring.Presets
import Style.Core as Core
import Svg exposing (Attribute)


{-| An Animation of CSS properties.
-}
type Animation
    = A Core.Model

type alias KeyframeWithOptions =
    { frame : Core.Keyframe
    , duration : Maybe Time
    , easing : Maybe (Float -> Float)
    , spring : Maybe Spring.Model
    }


{-| A Temporary type that is used in constructing actions.
-}
type alias PreAction =
    { frames : List KeyframeWithOptions
    , action : List Core.Keyframe -> Core.Action
    }

--type alias Action = Core.Action



--| Actions to be run on an animation.
--You won't be constructing this type directly, though it may show up in your type signatures.

--To start animations you'll be using the `animate` and `queue`

--type Action
--    = Staggered (Float -> Float -> Action)
--    | Unstaggered PreAction
--    | Internal Core.Action



emptyKeyframeWithOptions =
    { frame = Core.emptyKeyframe
    , duration = Nothing
    , easing = Nothing
    , spring = Nothing
    }


{-| Create an initial style for your init model.

__Note__ All properties that you animate must be present in the init or else that property won't be animated.

-}
init : Style -> Animation
init sty =
    let
        deduped =
            List.foldr
                (\x acc ->
                    if
                        List.any
                            (\y ->
                                Style.PropertyHelpers.id x
                                    == Style.PropertyHelpers.id y
                            )
                            acc
                    then
                        acc
                    else
                        x :: acc
                )
                []
                sty
        empty = Core.empty
    in
        A { empty | previous = deduped }





{-| Animate based on spring physics.

You'll need to provide both a stiffness and a dampness to this function.

__Note:__ This will cause both `duration` and `easing` to be ignored as they are now controlled by the spring.

     Style.animate
         -- |> Style.spring Style.noWobble -- set using a UI preset
         |> Style.spring
                { stiffness = 400
                , damping = 28
                }
         |> Style.to
             [ Left (Style.to 0) Px
             , Opacity (Style.to 1)
             ]
         |> Style.on model.style
-}
spring : Style.Spring.Presets.SpringProps -> PreAction -> PreAction
spring spring action =
    let
        newSpring =
            Just
                { destination = 1.0
                , damping = spring.damping
                , stiffness = spring.stiffness
                }
    in
        updateOrCreate action (\a -> { a | spring = newSpring })



{-| Update a style based on it's previous value.

     Style.animate
          |> Style.update
              (\index prev =
                  case prev of
                    Cx cx ->
                        Cx (cx + 1)

                    Cy cy ->
                        Cy (cy + 1)

                    _ -> prev
              )
          |> Style.on model.style

`index` is the number of times that property has occurred in the stack.
This can be useful when stacking transforms and you only want to update the 2nd `Rotate` in the stack.
Refer to the [stacking transforms example](https://github.com/mdgriffith/elm-style-animation) for more information.

-}
update : (Int -> Static -> Static) -> PreAction -> PreAction
update styleUpdate action =
         updateOrCreate action
            (\kfWithOptions ->
                let
                    frame =
                        kfWithOptions.frame

                    updatedFrame =
                        { frame | retarget = Just styleUpdate }
                in
                    { kfWithOptions | frame = updatedFrame }
            )


{-| Apply an update to a Animation model.

     Style.animate
         |> Style.duration (0.4*second)
         |> Style.to
             [ Left 0 Px
             , Opacity 1
             ]
         |> Style.on model.style

-}
on : Animation -> PreAction -> Animation
on (A model) preaction =
    let
        action =
            preaction.action
                <| List.map applyKeyframeOptions preaction.frames
    in
        A <| Core.update action model



{-| Begin describing an animation.  This animation will cleanly interrupt any animation that is currently running.

      Style.animate
         |> Style.duration (0.4*second)
         |> Style.to
             [ Left 0 Px
             , Opacity 1
             ]
         |> Style.on model.style

-}
animate : PreAction
animate =
    { frames = []
    , action = Core.Interrupt
    }

{-| The same as `animate` but instead of interrupting the current animation, this will queue up after the current animation is finished.

      Style.queue
         |> Style.duration (0.4*second)
         |> Style.to
             [ Left (Style.to 0) Px
             , Opacity (Style.to 1)
             ]
         |> Style.on model.style

-}
queue : PreAction
queue =
    { frames = []
    , action = Core.Queue
    }


{-| Step the animation
-}
tick : Float -> Animation -> Animation
tick time (A model) =
    A <| Core.update (Core.Tick time) model


applyKeyframeOptions : KeyframeWithOptions -> Core.Keyframe
applyKeyframeOptions options =
    let
        frame =
            options.frame

        applyOpt prop =
            let
                addOptions a =
                    let
                        newSpring =
                            case options.spring of
                                Nothing ->
                                    a.spring

                                Just partialSpring ->
                                    let
                                        oldSpring =
                                            a.spring
                                    in
                                        { oldSpring
                                            | stiffness = partialSpring.stiffness
                                            , damping = partialSpring.damping
                                        }

                        withEase =
                            Maybe.map
                                (\ease ->
                                    { emptyEasing | ease = ease }
                                )
                                options.easing

                        withDuration =
                            case options.duration of
                                Nothing ->
                                    withEase

                                Just dur ->
                                    case withEase of
                                        Nothing ->
                                            Just { emptyEasing | duration = dur }

                                        Just ease ->
                                            Just { ease | duration = dur }
                    in
                        { a
                            | spring = newSpring
                            , easing = withDuration
                        }
            in
                { prop
                    | current = Style.PropertyHelpers.update addOptions prop.current

                }


        newProperties =
            List.map applyOpt frame.properties
    in
        { frame | properties = newProperties }



{-| Apply a style immediately.  This takes a list of static style properties, meaning the no `Style.to` functions, only concrete numbers and values.


    Style.animate
         |> Style.duration (0.4*second)
         |> Style.to
             [ Opacity (Style.to 1)
             ]
      |> Style.andThen
         |> Style.set
             [ Display None
             ]
         |> Style.on model.style

-}
set : Style -> PreAction -> PreAction
set staticProps action =
    let
        actionWithProps = to staticProps action
    in
        updateOrCreate actionWithProps
            (\kfWithOpts ->
                { kfWithOpts
                    | duration = Just 0
                    , easing = Just (\x -> x)
                }
            )




{-| Specify a duration.  This is ignored unless an easing is specified as well!  This is because spring functions (the default), have dynamically created durations.

If an easing is specified but no duration, the default duration is 350ms.

     Style.animate
         |> Style.easing (\x -> x)  -- linear easing
         |> Style.duration (0.4*second)
         |> Style.to
             [ Left 0 Px
             , Opacity 1
             ]
         |> Style.on model.style
-}
duration : Time -> PreAction -> PreAction
duration dur action =
    updateOrCreate action (\a -> { a | duration = Just dur })


{-| Specify a delay.
If not specified, the default is 0.

     Style.animate
         |> Style.duration (0.4*second)
         |> Style.delay (0.5*second)
         |> Style.to
             [ Left 0 Px
             , Opacity 1
             ]
         |> Style.on model.style
-}
delay : Time -> PreAction -> PreAction
delay delay action =
    updateOrCreate action
        (\a ->
            let
                frame =
                    a.frame

                updatedFrame =
                    { frame | delay = delay }
            in
                { a | frame = updatedFrame }
        )


{-| Specify an easing function.  It is expected that values should match up at the beginning and end.  So, f 0 == 0 and f 1 == 1.  The default easing is sinusoidal in-out.

-}
easing : (Float -> Float) -> PreAction -> PreAction
easing ease action =
    updateOrCreate action (\a -> { a | easing = Just ease })


{-| Append another keyframe.  This is used for multistage animations.

For example, to cycle through colors, we'd use the following:

      Style.animate
          |> Style.to
              [ BackgroundColor
                    Style.toRGBA 100 100 100 1.0
              ]
          |> Style.andThen -- create a new keyframe
          |> Style.duration (1*second)
          |> Style.to
              [ BackgroundColor
                    Style.toRGBA 178 201 14 1.0
              ]
          |> Style.andThen
          |> Style.to
              [ BackgroundColor
                    Style.toRGBA 58 40 69 1.0
              ]
          |> Style.on model.style
-}
andThen : PreAction -> PreAction
andThen preaction =
        { preaction
            | frames = preaction.frames ++ [ emptyKeyframeWithOptions ]
        }


{-| Update the last Core.Keyframe in the queue.  If the queue is empty, create a new Core.Keyframe and update that.
-}
updateOrCreate : PreAction -> (KeyframeWithOptions -> KeyframeWithOptions) -> PreAction
updateOrCreate preaction fn =
               { preaction
                    | frames =
                        case List.reverse preaction.frames of
                            [] ->
                                [ fn emptyKeyframeWithOptions ]

                            cur :: rem ->
                                List.reverse ((fn cur) :: rem)
               }


{-| Animate to a statically specified style.

-}
to : Style -> PreAction -> PreAction
to sty action =
    let
        deduped =
            List.foldr
                (\x acc ->
                    if
                        List.any
                            (\y ->
                                Style.PropertyHelpers.id x
                                    == Style.PropertyHelpers.id y
                                    && Style.PropertyHelpers.id x /= "transform"
                            )
                            acc
                    then
                        acc
                    else
                        x :: acc
                )
                []
                sty

        dynamicProperties =
            List.map
                (\prop ->
                    { target = prop
                    , current = Style.PropertyHelpers.toDynamic prop
                    }
                )
                deduped

    in
     updateOrCreate action
        (\kfWithOptions ->
            let
                frame =
                    kfWithOptions.frame

                updatedFrame =
                    { frame | properties = dynamicProperties }
            in
                { kfWithOptions | frame = updatedFrame }
        )


{-| Render into concrete css that can be directly applied to 'style' in elm-html

    div [ style Style.render widget.style) ] [ ]

-}
render : Animation -> List ( String, String )
render (A model) =
    case List.head model.frames of
        Nothing ->
            Style.PropertyHelpers.render model.previous

        Just frame ->
            Style.PropertyHelpers.render <| Core.bake frame model.previous


{-| Render into svg attributes.

    polygon (Style.renderAttr widget.style) [ ]

-}
renderAttr : Animation -> List (Attribute msg)
renderAttr (A model) =
    case List.head model.frames of
        Nothing ->
            Style.PropertyHelpers.renderAttr model.previous

        Just frame ->
            Style.PropertyHelpers.renderAttr <| Core.bake frame model.previous
