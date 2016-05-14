module Style exposing (Animation, Action, init, update, render, renderAttr, animate, queue, stagger, on, delay, duration, easing, spring, andThen, set, tick, to, noWobble, gentle, wobbly, stiff, fromColor, rgb, rgba, hsl, hsla)

{-| This library is for animating css properties and is meant to work well with elm-html.

The easiest way to get started with this library is to check out the examples that are included with the [source code](https://github.com/mdgriffith/elm-html-animation).

Once you have the basic structure of how to use this library, you can refer to this documentation to fill any gaps.


# Base Definitions
@docs Animation, Action

# Starting an animation
@docs animate, queue, stagger

# Creating animations
@docs delay, spring, duration, easing, andThen

# Animating Properties

These functions specify the value for a Property.

After taking an argument, these functions have `Float -> Float -> Float` as their signature.
This can be understood as `ExistingStyleValue -> CurrentTime -> NewStyleValue`, where CurrentTime is between 0 and 1.

@docs to, set

# Spring Presets
@docs noWobble, gentle, wobbly, stiff

# Render a Animation into CSS or as SVG attributes
@docs render, renderAttr

# Setting the starting style
@docs init

# Initial Color Formats
@docs fromColor, rgb, rgba, hsl, hsla

# Update a Style
@docs update

# Managing Commands
@docs on, tick

-}

import Time exposing (Time, second)
import String exposing (concat)
import List
import Color
import Style.Properties
import Style.Spring as Spring
import Style.Core as Core exposing (Static)


{-| An Animation of CSS properties.
-}
type Animation
    = A Core.Model

type alias Property a = Style.Properties.Property a


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


type alias Dynamic =
    Core.Physics


{-| Actions to be run on an animation.
You won't be constructing this type directly, though it may show up in your type signatures.

To start animations you'll be using the `animate`, `queue`, and `stagger` functions
-}
type Action
    = Staggered (Float -> Float -> Action)
    | Unstaggered PreAction
    | Internal Core.Action



emptyKeyframeWithOptions =
    { frame = Core.emptyKeyframe
    , duration = Nothing
    , easing = Nothing
    , spring = Nothing
    }


{-| Create an initial style for your init model.

__Note__ All properties that you animate must be present in the init or else that property won't be animated.

-}
init : Core.Style -> Animation
init sty =
    let
        deduped =
            List.foldr
                (\x acc ->
                    if
                        List.any
                            (\y ->
                                Style.Properties.id x
                                    == Style.Properties.id y
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


type alias SpringProps =
    { stiffness : Float
    , damping : Float
    }


{-| Animate based on spring physics.

You'll need to provide both a stiffness and a dampness to this function.

__Note:__ This will cause both `duration` and `easing` to be ignored as they are now controlled by the spring.

     UI.animate
         -- |> UI.spring UI.noWobble -- set using a UI preset
         |> UI.spring
                { stiffness = 400
                , damping = 28
                }
         |> UI.props
             [ Left (UI.to 0) Px
             , Opacity (UI.to 1)
             ]
         |> UI.on model.style
-}
spring : SpringProps -> Action -> Action
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


{-| A spring preset.  Probably should be your initial goto for using springs.
-}
noWobble : SpringProps
noWobble =
    { stiffness = 170
    , damping = 26
    }


{-| A spring preset.
-}
gentle : SpringProps
gentle =
    { stiffness = 120
    , damping = 14
    }


{-| A spring preset.
-}
wobbly : SpringProps
wobbly =
    { stiffness = 180
    , damping = 12
    }


{-| A spring preset.
-}
stiff : SpringProps
stiff =
    { stiffness = 210
    , damping = 20
    }

{-| Update a style based on it's previous value

     Style.animate 
          |> Style.update 
              (\prev =
                  case prev of 
                    Cx cx ->
                        Cx (cx + 1)

                    Cy cy ->
                        Cy (cy + 1)

                    _ -> prev
              )
          |> Style.on model.style
-}
update : (Style.Properties.Property Float -> Style.Properties.Property Float) -> Action -> Action
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


{-| Apply an update to a Animation model.  This is used at the end of constructing an animation.
However, you'll have an overall cleaner syntax if you use `forwardTo` to prepare a custom version of `on`.

     UI.animate
         |> UI.duration (0.4*second)
         |> UI.to
             [ Left (UI.to 0) Px
             , Opacity (UI.to 1)
             ]
         |> UI.on model.style

-}
on : Animation -> Action -> Animation
on (A model) action =
     A <| Core.update (resolve action 1 0) model



{-| Begin describing an animation.  This animation will cleanly interrupt any animation that is currently running.

      UI.animate
         |> UI.duration (0.4*second)
         |> UI.to
             [ Left 0 Px
             , Opacity 1
             ]
         |> UI.on model.style

-}
animate : Action
animate =
    Unstaggered
        <| { frames = []
           , action = Core.Interrupt
           }


{-| Step the animation
-}
tick : Float -> Animation -> Animation
tick time model =
    on model (Internal (Core.Tick time))


{-| The same as `animate` but instead of interrupting the current animation, this will queue up after the current animation is finished.

      UI.queue
         |> UI.duration (0.4*second)
         |> UI.props
             [ Left (UI.to 0) Px
             , Opacity (UI.to 1)
             ]
         |> UI.on model.style

-}
queue : Action
queue =
    Unstaggered
        <| { frames = []
           , action = Core.Queue
           }


{-| Can be used to stagger animations on a list of widgets.

     UI.stagger
        (\i ->
           UI.animate
             -- The delay is staggered based on list index
             |> UI.delay (i * 0.05 * second)
             |> UI.duration (0.3 * second)
             |> UI.props
                 [ Left (UI.to 200) Px
                 ]
          |> UI.andThen
             |> UI.delay (2.0 * second)
             |> UI.duration (0.3 * second)
             |> UI.props
                 [ Left (UI.to -50) Px
                 ]
        )
        |> forwardToAllWidgets model.widgets

-}
stagger : (Float -> Float -> Action) -> Action
stagger =
    Staggered




{-| Resolve the stagger if there is one, and apply springs if present.

-}
resolve : Action -> Int -> Int -> Core.Action
resolve stag t i =
    case stag of
        Unstaggered preaction ->
            preaction.action
                <| List.map applyKeyframeOptions
                    preaction.frames

        Staggered s ->
            resolve (s (toFloat t) (toFloat i)) t i

        Internal ia ->
            ia



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

                        newEasing =
                            Core.emptyEasing

                        withEase =
                            Maybe.map
                                (\ease ->
                                    { newEasing | ease = ease }
                                )
                                options.easing

                        withDuration =
                            case options.duration of
                                Nothing ->
                                    withEase

                                Just dur ->
                                    case withEase of
                                        Nothing ->
                                            Just { newEasing | duration = dur }

                                        Just ease ->
                                            Just { ease | duration = dur }
                    in
                        { a
                            | spring = newSpring
                            , easing = withDuration
                        }
            in
                { prop 
                    | current = Style.Properties.map addOptions prop.current

                } 
                

        newProperties = 
            List.map applyOpt frame.properties
    in
        { frame | properties = newProperties }



{-| Apply a style immediately.  This takes a list of static style properties, meaning the no `UI.to` functions, only concrete numbers and values.


    UI.animate
         |> UI.duration (0.4*second)
         |> UI.props
             [ Opacity (UI.to 1)
             ]
      |> UI.andThen
         |> UI.set
             [ Display None
             ]
         |> UI.on model.style

-}
set : Core.Style -> Action -> Action
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

     UI.animate
         |> UI.easing (\x -> x)  -- linear easing
         |> UI.duration (0.4*second)
         |> UI.props
             [ Left (UI.to 0) Px
             , Opacity (UI.to 1)
             ]
         |> UI.on model.style
-}
duration : Time -> Action -> Action
duration dur action =
    updateOrCreate action (\a -> { a | duration = Just dur })


{-| Specify a delay.
If not specified, the default is 0.

     UI.animate
         |> UI.duration (0.4*second)
         |> UI.delay (0.5*second)
         |> UI.props
             [ Left (UI.to 0) Px
             , Opacity (UI.to 1)
             ]
         |> UI.on model.style
-}
delay : Time -> Action -> Action
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
easing : (Float -> Float) -> Action -> Action
easing ease action =
    updateOrCreate action (\a -> { a | easing = Just ease })


{-| Append another keyframe.  This is used for multistage animations.

For example, to cycle through colors, we'd use the following:

      UI.animate
              |> UI.props
                  [ BackgroundColor
                        UI.toRGBA 100 100 100 1.0
                  ]
          |> UI.andThen -- create a new keyframe
              |> UI.duration (1*second)
              |> UI.props
                  [ BackgroundColor
                        UI.toRGBA 178 201 14 1.0
                  ]
          |> UI.andThen
              |> UI.props
                  [ BackgroundColor
                        UI.toRGBA 58 40 69 1.0
                  ]
          |> UI.on model.style
-}
andThen : Action -> Action
andThen stag =
    case stag of
        Internal ia ->
            Internal ia

        Staggered s ->
            Staggered s

        Unstaggered preaction ->
            Unstaggered
                <| { preaction | frames = preaction.frames ++ [ emptyKeyframeWithOptions ] }


{-| Update the last Core.Keyframe in the queue.  If the queue is empty, create a new Core.Keyframe and update that.
-}
updateOrCreate : Action -> (KeyframeWithOptions -> KeyframeWithOptions) -> Action
updateOrCreate action fn =
    case action of
        Internal ia ->
            Internal ia

        Staggered s ->
            Staggered s

        Unstaggered preaction ->
            Unstaggered
                <| { preaction
                    | frames =
                        case List.reverse preaction.frames of
                            [] ->
                                [ fn emptyKeyframeWithOptions ]

                            cur :: rem ->
                                List.reverse ((fn cur) :: rem)
                   }


{-| Animate to a statically specified style.

-}
to : Core.Style -> Action -> Action
to sty action =
    let
        deduped =
            List.foldr
                (\x acc ->
                    if
                        List.any
                            (\y ->
                                Style.Properties.id x
                                    == Style.Properties.id y
                                    && Style.Properties.id x /= "transform"
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
                    , current = Style.Properties.map (\_ -> Core.emptyPhysics) prop
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


{-| Specify an initial Color-based property using a Color from the elm core Color module.

-}
fromColor : Color.Color -> (Static -> Static -> Static -> Static -> Property Static) -> Property Static
fromColor color almostColor =
    let
        rgba =
            Color.toRgb color
    in
        almostColor (toFloat rgba.red)
            (toFloat rgba.green)
            (toFloat rgba.blue)
            (rgba.alpha)


{-| Specify an initial Color-based property using rgb

-}
rgb : Float -> Float -> Float -> (Static -> Static -> Static -> Static -> Property Static) -> Property Static
rgb r g b prop =
    prop r g b 1.0


{-| Specify an initial Color-based property using rgba

-}
rgba : Float -> Float -> Float -> Float -> (Static -> Static -> Static -> Static -> Property Static) -> Property Static
rgba r g b a prop =
    prop r g b a


{-| Specify an initial Color-based property using hsl

-}
hsl : Float -> Float -> Float -> (Static -> Static -> Static -> Static -> Property Static) -> Property Static
hsl h s l prop =
    let
        rgba =
            Color.toRgb <| Color.hsl h s l
    in
        prop (toFloat rgba.red)
            (toFloat rgba.blue)
            (toFloat rgba.green)
            rgba.alpha


{-| Specify an initial Color-based property using hsla

-}
hsla : Float -> Float -> Float -> Float -> (Static -> Static -> Static -> Static -> Property Static) -> Property Static
hsla h s l a prop =
    let
        rgba =
            Color.toRgb <| Color.hsla h s l a
    in
        prop (toFloat rgba.red)
            (toFloat rgba.blue)
            (toFloat rgba.green)
            rgba.alpha


{-| Render into concrete css that can be directly applied to 'style' in elm-html

    div [ style UI.render widget.style) ] [ ]

-}
render : Animation -> List ( String, String )
render (A model) =
    case List.head model.frames of
        Nothing ->
            Style.Properties.render model.previous

        Just frame ->
            Style.Properties.render <| Core.bake frame model.previous


{-| Render into svg attributes.

    div (Style.renderAttr widget.style) [ ]

-}
renderAttr (A model) =
    case List.head model.frames of
        Nothing ->
            Style.Properties.renderAttr model.previous

        Just frame ->
            Style.Properties.renderAttr <| Core.bake frame model.previous



