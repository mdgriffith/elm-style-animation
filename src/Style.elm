module Style
    exposing
        ( Animation
        , Options
        , init
        , initWith
          -- , update
        , render
        , attrs
        , animate
        , queue
        , forever
        , on
        , wait
          -- , spring
        , set
        , tick
        , to
        )

{-| This library is for animating css properties and is meant to work well with elm-html.

The easiest way to get started with this library is to check out the examples that are included with the [source code](https://github.com/mdgriffith/elm-style-animation).

Once you have the basic structure of how to use this library, you can refer to this documentation to fill any gaps.


# Base Definitions
@docs Animation

# Starting an animation
@docs animate, queue, forever

# Creating animations
@docs wait

# Animating Properties
@docs to, set, update

# Render a Animation into CSS or as SVG attributes
@docs render, attrs

# Set the starting style
@docs init

# Managing Commands
@docs on, tick

-}

import Time exposing (Time, second)
import String exposing (concat)
import List
import Color
import Style.Properties exposing (Property(..))
import Style.PropertyHelpers exposing (Style, emptyEasing, Static, id, apply)
import Style.Spring as Spring
import Style.Spring.Presets
import Style.Core as Core
import Style.Collection
import Svg exposing (Attribute)
import Html.Attributes


{-| An Animation of CSS properties.
-}
type Animation
    = A Core.Model


{-| A Temporary type that is used in constructing actions.
-}
type alias PreAction =
    { frames : List Core.Keyframe
    , action : List Core.Keyframe -> Core.Action
    }


{-| For use when defining animations that repeat forever.

  import Style exposing (forever)

  Style.repeat forever
       |> Style.update
              [ Rotate ((+) 1) Turn
              ]
       |> Style.on model.style

-}
forever : Float
forever =
    1 / 0


{-| Create an initial style for your init model.

__Note__ All properties that you animate must be present in the init or else that property won't be animated.

-}
init : Style -> Animation
init style =
    A <| Core.init style


{-| An Options object that can be used to set the defaults that are used.
-}
type alias Options =
    { spring : Spring.Preset }


{-| Initialize a style with custom defaults such as the default spring model to use.
-}
initWith : Options -> Style -> Animation
initWith options style =
    let
        model =
            Core.init style

        newDefaults =
            model.defaults

        withSpring =
            { newDefaults | spring = options.spring }
    in
        A <| { model | defaults = withSpring }



-- {-| Animate based on spring physics.
--
-- You'll need to provide both a stiffness and damping to this function.
--
-- __Note:__ This will cause both `duration` and `easing` to be ignored as they are now controlled by the spring.
--
--      Style.animate
--       -- |> Style.spring Style.noWobble -- set using a UI preset
--          |> Style.spring { stiffness = 400, damping = 28 }
--              [ Left 0 Px
--              , Opacity 1
--              ]
--          |> Style.on model.style
-- -}
-- spring : Style.Spring.Presets.SpringProps -> Style -> PreAction -> PreAction
-- spring spring sty action =
--     let
--         newSpring =
--             Just
--                 { destination = 1.0
--                 , damping = spring.damping
--                 , stiffness = spring.stiffness
--                 }
--     in
--         { action | frames = preAction.frames ++ [ Core.To sty ] }


{-| Update a style based on it's previous value.

     Style.animate
          |> Style.update
              [ Cx ((+) 1)
              , Cy ((+) 1)
              , Color greyscale
              ]
          |> Style.on model.style

-}
update : List Style.PropertyHelpers.Retarget -> PreAction -> PreAction
update props preAction =
    { preAction
        | frames = preAction.frames ++ [ Core.Update props ]
    }


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
        corrected =
            List.map
                (\frame ->
                    case frame of
                        Core.Wait t ->
                            Core.Wait <| t + model.times.current

                        f ->
                            f
                )
                preaction.frames
    in
        A <| Core.update (preaction.action corrected) model


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


nudge : Style -> Animation -> Animation
nudge style (A model) =
    A <| Core.update (Core.Nudge style) model



--
-- {-| Interrupt the current animation and begin repeating the specified one.
--
--       Style.repeat 3
--          |> Style.duration (0.4*second)
--          |> Style.to
--              [ Left (Style.to 0) Px
--              , Opacity (Style.to 1)
--              ]
--          |> Style.on model.style
--
-- You can repeat forever by providing Infinity as the number.
--
--       Style.repeat forever
--          |> Style.duration (0.4*second)
--          |> Style.to
--              [ Left (Style.to 0) Px
--              , Opacity (Style.to 1)
--              ]
--          |> Style.on model.style
--
--
-- -}
-- repeat : Float -> PreAction
-- repeat i =
--     { frames = []
--     , action = Core.Repeat i
--     }
--
--
-- {-| Start a repeating animation after the current animation has finished.
--
--       Style.queueRepeat 3
--          |> Style.duration (0.4*second)
--          |> Style.to
--              [ Left (Style.to 0) Px
--              , Opacity (Style.to 1)
--              ]
--          |> Style.on model.style
--
-- You can repeat forever by providing Infinity as the number.
--
--       Style.queueRepeat forever
--          |> Style.duration (0.4*second)
--          |> Style.to
--              [ Left (Style.to 0) Px
--              , Opacity (Style.to 1)
--              ]
--          |> Style.on model.style
--
--
-- -}
-- queueRepeat : Float -> PreAction
-- queueRepeat i =
--     { frames = []
--     , action = Core.QueueRepeat i
--     }


{-| Step the animation
-}
tick : Float -> Animation -> Animation
tick time (A model) =
    A <| Core.update (Core.Tick time) model


{-| Apply a style immediately.

    Style.animate
         |> Style.duration (0.4*second)
         |> Style.to
             [ Opacity 1
             ]
         |> Style.set
             [ Display None
             ]
         |> Style.on model.style

-}
set : Style -> PreAction -> PreAction
set props preAction =
    { preAction
        | frames = preAction.frames ++ [ Core.Set props ]
    }



-- {-| Specify a duration.  This is ignored unless an easing is specified as well!  This is because spring functions (the default), have dynamically created durations.
--
-- If an easing is specified but no duration, the default duration is 350ms.
--
--      Style.animate
--          |> Style.easing (\x -> x)  -- linear easing
--          |> Style.duration (0.4*second)
--          |> Style.to
--              [ Left 0 Px
--              , Opacity 1
--              ]
--          |> Style.on model.style
-- -}
-- duration : Time -> PreAction -> PreAction
-- duration dur action =
--     updateOrCreate action (\a -> { a | duration = Just dur })


{-| Wait until applying the next keyframe.

     Style.animate
         |> Style.wait (0.5*second)
         |> Style.to
             [ Left 0 Px
             , Opacity 1
             ]
         |> Style.on model.style
-}
wait : Time -> PreAction -> PreAction
wait delay preAction =
    { preAction
        | frames = preAction.frames ++ [ Core.Wait delay ]
    }



-- {-| Specify an easing function.  It is expected that values should match up at the beginning and end.  So, f 0 == 0 and f 1 == 1.  The default easing is sinusoidal in-out.
--
-- -}
-- easing : (Float -> Float) -> PreAction -> PreAction
-- easing ease action =
--     updateOrCreate action (\a -> { a | easing = Just ease })
--
--


{-| Animate to a statically specified style.

-}
to : Style -> PreAction -> PreAction
to sty preAction =
    { preAction | frames = preAction.frames ++ [ Core.To sty ] }


{-| Apply style properties as an inline style.


    div [ style <| Style.render widget.style ] [ ]

*Note* - This will not capture properties that cannot be represented as an inline style.
For example, in svg, the `d` property for a `path` element has to be rendered as an attribute.
In this case you'd want to use `Style.attrs` instead.

-}
render : Animation -> List ( String, String )
render (A model) =
    Style.PropertyHelpers.render <| Style.Collection.bake model.current model.previous


{-| Apply style properties as an inline style and assign svg attributes as necessary.

For example, the 'd' svg attribute for the path element can only be rendered as an attribute.

Long story short, use `Style.attrs` when using SVG, and `Style.render` when you only want an inline style.

    polygon (Style.attrs widget.style) [ ]

-}
attrs : Animation -> List (Attribute msg)
attrs animation =
    (Html.Attributes.style <| render animation) :: renderAttr animation


renderAttr : Animation -> List (Attribute msg)
renderAttr (A model) =
    Style.PropertyHelpers.renderAttr <| Style.Collection.bake model.current model.previous
