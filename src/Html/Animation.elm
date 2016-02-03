module Html.Animation (Animation, Action, init, update, render, animate, queue, stagger, on, props, delay, andThen, forwardTo, forwardToAll, to, add, minus, stay, noWobble, gentle, wobbly, stiff, fastAndLoose, toColor, toRGB, toRGBA, toHSL, toHSLA, fromColor, rgb, rgba, hsl, hsla) where

{-| This library is for animating css properties and is meant to work well with elm-html.

The easiest way to get started with this library is to check out the examples that are included with the [source code](https://github.com/mdgriffith/elm-html-animation).

Once you have the basic structure of how to use this library, you can refer to this documentation to fill any gaps.


# Base Definitions
@docs Animation, Action

# Creating an animation
@docs animate, queue, stagger, props, delay, andThen, on

# Animating Properties

These functions specify the value for a StyleProperty.

After taking an argument, these functions have `Float -> Float -> Float` as their signature.
This can be understood as `ExistingStyleValue -> CurrentTime -> NewStyleValue`, where CurrentTime is between 0 and 1.

@docs to, stay, add, minus

# Spring Presets
@docs noWobble, gentle, wobbly, stiff, fastAndLoose

# Animating Colors
@docs toColor, toRGB, toRGBA, toHSL, toHSLA

# Render a Animation into CSS
@docs render

# Setting the starting style
@docs init

# Initial Color Formats
@docs fromColor, rgb, rgba, hsl, hsla

# Update a Style
@docs update


# Managing a list of styled widgets
@docs forwardTo, forwardToAll

-}

import Effects exposing (Effects)
import Time exposing (Time, second)
import String exposing (concat)
import List
import Color

import Html.Animation.Properties exposing (..)
import Html.Animation.Render as Render
import Html.Animation.Spring as Spring



type alias Model =
  { start : Maybe Time
  , elapsed : Time
  , anim : List StyleKeyframe
  , previous : Style
  }


{-| An Animation of CSS properties.
-}
type Animation
  = A Model

type alias DynamicTarget =
  Float -> Float -> Float

type alias Physics a =
  { target : a
  , position : Float
  , spring : Spring.Model
  , easing : Maybe Easing
  }


type alias Easing =
  { ease : Float -> Float
  , counter : Maybe Spring.Model
  , duration : Time
  }


{-| Represent a CSS style as a list of style properties with concrete values.
-}
type alias Style =
  List (StyleProperty Static)


{-| Represent a style animation.
This is a list of StylePropertys, but instead of having a static value like '5',
it has a function that takes the previous value, the current time, and provides the current value.
-}
type alias StyleKeyframe =
  { target : List (StyleProperty (Physics DynamicTarget))
  , delay : Time
  }


type alias KeyframeWithOptions =
        { frame : StyleKeyframe
        , duration : Maybe Time
        , easing : Maybe (Float -> Float)
        , spring : Maybe Spring.Properties
        }



{-| -}
type InternalAction
  = Queue (List StyleKeyframe)
  | Interrupt (List StyleKeyframe)
  | Tick Time


{-| Actions to be run on an animation.
You won't be constructing using this type directly, though it may show up in your type signatures.

To start animations you'll be using the `animate`, `queue`, and `stagger` functions
-}

type alias PreAction = 
        { frames : List KeyframeWithOptions
        , action : (List StyleKeyframe -> InternalAction)
        }


type Action
      = Staggered (Float -> Action)
      | Unstaggered PreAction
      | Internal InternalAction


-- private


empty : Model
empty =
  { elapsed = 0.0
  , start = Nothing
  , anim = []
  , previous = []
  }



-- private


emptyKeyframe : StyleKeyframe
emptyKeyframe =
  { target = []
  , delay = 0.0
  }


emptyPhysics : a -> Physics a
emptyPhysics target =
  { target = target
  , position = 0
  , spring =
      { velocity = 0
      , position = 0
      , stiffness = noWobble.stiffness
      , damping = noWobble.damping
      , destination = 1
      }
  , easing = Nothing
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
                Render.id x
                  == Render.id y
                  && Render.name x
                  /= "transform"
              )
              acc
          then
            acc
          else
            x :: acc
        )
        []
        sty
  in
    A { empty | previous = deduped }



-- private


defaultDuration : Float
defaultDuration =
  0.35 * second



-- private


defaultEasing : Float -> Float
defaultEasing x =
  (1 - cos (pi * x)) / 2


{-| Update an animation.  This will probably only show up once in your code.
See any of the examples at [https://github.com/mdgriffith/elm-html-animation](https://github.com/mdgriffith/elm-html-animation)
-}
update : Action -> Animation -> ( Animation, Effects Action )
update action anim =
  let
    ( anim, fx ) =
      internalUpdate (resolve action 0) anim
  in
    ( anim, Effects.map Internal fx )


internalUpdate : InternalAction -> Animation -> ( Animation, Effects InternalAction )
internalUpdate action (A model) =
  case action of
    Queue anims ->
      ( A { model | anim = model.anim ++ anims }
      , Effects.tick Tick
      )

    Interrupt anims ->
      -- Only interrupt if anims end in different states.
      --if equivalentAnim model.previous model.anim anims then
      --    ( A model, Effects.none )
      --else
      let
        currentAnim =
          List.head model.anim

        previous =
          case currentAnim of
            Nothing ->
              model.previous

            Just frame ->
              bake frame model.previous
      in
        ( A
            { model
              | anim = mapTo 0 (\a -> step a previous 0.0 0.0) anims
              , elapsed = 0.0
              , start = Nothing
              , previous = previous
            }
        , Effects.tick Tick
        )

    Tick now ->
      let
        start =
          case model.start of
            Nothing ->
              now

            Just t ->
              t

        newElapsed =
          now - start

        currentAnim =
          List.head model.anim

        remaining =
          List.tail model.anim
      in
        case currentAnim of
          Nothing ->
            ( A
                { model
                  | elapsed = 0.0
                  , start = Nothing
                  , previous = model.previous
                  , anim = model.anim
                }
            , Effects.none
            )

          Just current ->
            if done newElapsed current then
              let
                anims =
                  case remaining of
                    Nothing ->
                      []

                    Just a ->
                      a

                previous =
                  bake current model.previous

                resetElapsed =
                  newElapsed

                --newElapsed - (current.duration + current.delay)
              in
                ( A
                    { model
                      | elapsed = resetElapsed
                      , start = Just (now - resetElapsed)
                      , previous = previous
                      , anim = mapTo 0 (\a -> step a previous resetElapsed resetElapsed) anims
                    }
                , Effects.tick Tick
                )
            else
              ( A
                  { model
                    | elapsed = newElapsed
                    , start = Just start
                    , anim = mapTo 0 (\a -> step a model.previous newElapsed (newElapsed - model.elapsed)) model.anim
                  }
              , Effects.tick Tick
              )



--finalStyle : Style -> List StyleKeyframe -> Style
--finalStyle style keyframes =
--                List.foldl
--                      (\frame st ->
--                        bakeFinal frame st
--                      ) style keyframes
--equivalentAnim : Style -> List StyleKeyframe -> List StyleKeyframe -> Bool
--equivalentAnim style frame1 frame2 =
--                        if List.length frame1 == 0 then
--                          False
--                        else
--                          let
--                            final1 = finalStyle style frame1
--                            final2 = finalStyle style frame2
--                          in
--                            final1 == final2


done : Time -> StyleKeyframe -> Bool
done time frame =
  List.all (propDone time) frame.target


propDone : Time -> StyleProperty (Physics DynamicTarget) -> Bool
propDone time prop =
  let
    isDone prop =
      case prop.easing of
        Nothing ->
          Spring.atRest prop.spring

        Just easing ->
          easing.ease time
            == 1.0
            && easing.counter
            == Nothing
  in
    case prop of
      Prop _ a _ ->
        isDone a

      Opacity a ->
        isDone a

      Height a _ ->
        isDone a

      Width a _ ->
        isDone a

      Left a _ ->
        isDone a

      Top a _ ->
        isDone a

      Right a _ ->
        isDone a

      Bottom a _ ->
        isDone a

      MaxHeight a _ ->
        isDone a

      MaxWidth a _ ->
        isDone a

      MinHeight a _ ->
        isDone a

      MinWidth a _ ->
        isDone a

      Padding a _ ->
        isDone a

      PaddingLeft a _ ->
        isDone a

      PaddingRight a _ ->
        isDone a

      PaddingTop a _ ->
        isDone a

      PaddingBottom a _ ->
        isDone a

      Margin a _ ->
        isDone a

      MarginLeft a _ ->
        isDone a

      MarginRight a _ ->
        isDone a

      MarginTop a _ ->
        isDone a

      MarginBottom a _ ->
        isDone a

      BorderWidth a _ ->
        isDone a

      BorderRadius a _ ->
        isDone a

      BorderTopLeftRadius a _ ->
        isDone a

      BorderTopRightRadius a _ ->
        isDone a

      BorderBottomLeftRadius a _ ->
        isDone a

      BorderBottomRightRadius a _ ->
        isDone a

      LetterSpacing a _ ->
        isDone a

      LineHeight a _ ->
        isDone a

      BackgroundPosition x y _ ->
        isDone x && isDone y

      TransformOrigin x y z _ ->
        isDone x && isDone y && isDone z

      Color x y z a ->
        isDone x && isDone y && isDone z && isDone a

      BackgroundColor x y z a ->
        isDone x && isDone y && isDone z && isDone a

      BorderColor x y z a ->
        isDone x && isDone y && isDone z && isDone a

      Translate a1 a2 _ ->
        isDone a1 && isDone a2

      Translate3d a1 a2 a3 _ ->
        isDone a1 && isDone a2 && isDone a3

      TranslateX a _ ->
        isDone a

      TranslateY a _ ->
        isDone a

      Scale a1 ->
        isDone a1

      Scale3d a1 a2 a3 ->
        isDone a1 && isDone a2 && isDone a3

      ScaleX a ->
        isDone a

      ScaleY a ->
        isDone a

      ScaleZ a ->
        isDone a

      Rotate a _ ->
        isDone a

      Rotate3d a1 a2 a3 a4 _ ->
        isDone a1 && isDone a2 && isDone a3 && isDone a4

      RotateX a _ ->
        isDone a

      RotateY a _ ->
        isDone a

      Skew a1 a2 _ ->
        isDone a1 && isDone a2

      SkewX a _ ->
        isDone a

      SkewY a _ ->
        isDone a

      Perspective a ->
        isDone a

      Matrix a b c x y z ->
        List.all isDone [ a, b, c, x, y, z ]

      Matrix3d a b c d e f g h i j k l m n o p ->
        List.all isDone [ a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p ]


{-| Begin describing an animation.  This animation will cleanly interrupt any animation that is currently running.

      UI.animate
         |> UI.duration (0.4*second)
         |> UI.props
             [ UI.Left UI.Px (UI.to 0)
             , UI.Opacity (UI.to 1)
             ]
         |> UI.on model.style

-}
animate : Action
animate = Unstaggered <|
    { frames = []
    , action = Interrupt
    }



{-| The same as `animate` but instead of interrupting the current animation, this will queue up after the current animation is finished.

      UI.queue
         |> UI.duration (0.4*second)
         |> UI.props
             [ UI.Left UI.Px (UI.to 0)
             , UI.Opacity (UI.to 1)
             ]
         |> UI.on model.style

-}
queue : Action
queue = Unstaggered <|
    { frames = []
    , action = Queue
    }


{-| Can be used to stagger animations on a list of widgets.

     UI.stagger
        (\i ->
           UI.animate
             |> UI.delay (i * 0.05 * second) -- The delay is staggered based on list index
             |> UI.duration (0.3 * second)
             |> UI.props
                 [ UI.Left (UI.to 200) UI.Px
                 ]
          |> UI.andThen
             |> UI.delay (2.0 * second)
             |> UI.duration (0.3 * second)
             |> UI.props
                 [ UI.Left (UI.to -50) UI.Px
                 ]
        )
        |> forwardToAllWidgets model.widgets

-}
stagger : (Float -> Action) -> Action
stagger =
  Staggered


{-| Apply an update to a Animation model.  This is used at the end of constructing an animation.

     UI.animate
         |> UI.duration (0.4*second)
         |> UI.props
             [ UI.Left UI.Px (UI.to 0)
             , UI.Opacity (UI.to 1)
             ]
         |> UI.on model.style

-}
on : Animation -> Action -> ( Animation, Effects Action )
on model action =
  update action model


{-| Resolve the stagger if there is one, and apply springs if present.

-}
resolve : Action -> Int -> InternalAction
resolve stag i =
  let
    f =
      toFloat i
  in
    case stag of
      Unstaggered preaction ->
        preaction.action <| 
            List.map 
                applyKeyframeOptions
                    preaction.frames

      Staggered s ->
        resolve (s f) i

      Internal ia ->
        ia


applyKeyframeOptions : KeyframeWithOptions -> StyleKeyframe
applyKeyframeOptions options = 
                let
                    frame = options.frame
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
                                                    oldSpring = a.spring
                                                in
                                                    { oldSpring 
                                                        | stiffness = partialSpring.stiffness
                                                        , damping = partialSpring.damping
                                                    }
                                in
                                    { a | spring = newSpring }
                        in
                            mapProp addOptions prop


                in
                    { frame | target = List.map applyOpt frame.target }

emptyKeyframeWithOptions =
        { frame = emptyKeyframe
        , duration = Nothing
        , easing = Nothing
        , spring = Nothing
        }



{-| Can be used in place of `on`.  Instead of applying an update directly to a Animation model,
you can forward the update to a specific element in a list that has a Animation model.

To use this function, you'll need to supply a getter and a setter function for getting and setting the style model.

So, for a model like the following

    type alias Model = { widgets : List Widget }

    type alias Widget =
              { style : UI.Animation
              }
You'd probably want to create a specialized version of `forwardTo`.

    forwardToWidget = UI.forwardTo
                        .style -- widget style getter
                        (\w style -> { w | style = style }) -- widget style setter

Which you can then use to apply an animation to a widget in a list.

    (widgets, fx) =
            UI.animate
                |> UI.duration (5*second)
                |> UI.props
                    [ UI.Opacity (UI.to 0)
                    ]
                |> forwardToWidget i model.widgets
                -- Where i is the index of the widget to update.

-}
forwardTo : (Int -> Action -> b) -> (a -> Animation) -> (a -> Animation -> a) -> Int -> List a -> Action -> ( List a, Effects b )
forwardTo toInternalAction styleGet styleSet i widgets action =
  let
    ( widgets, effects ) =
      List.unzip
        <| List.indexedMap
            (\j widget ->
              if j == i then
                let
                  ( newStyle, fx ) =
                    internalUpdate
                      (resolve action i)
                      (styleGet widget)
                in
                  ( styleSet widget newStyle
                  , Effects.map
                      (\a -> toInternalAction i (Internal a))
                      fx
                  )
              else
                ( widget, Effects.none )
            )
            widgets
  in
    ( widgets, Effects.batch effects )


{-| Same as `forwardTo`, except it applies an update to every member of the list.

-}
forwardToAll : (Int -> Action -> b) -> (a -> Animation) -> (a -> Animation -> a) -> List a -> Action -> ( List a, Effects b )
forwardToAll toInternalAction styleGet styleSet widgets action =
  let
    --largestDuration = List.map
    --                      (\i ->
    --                        case resolve action i of
    --                          Queue frames -> getFullDuration frames
    --                          Interrupt frames -> getFullDuration frames
    --                          _ -> 0.0
    --                      )
    --                      [1..List.length widgets]
    --                |> List.maximum
    --                |> Maybe.withDefault 0.0
    ( widgets, effects ) =
      List.unzip
        <| List.indexedMap
            (\i widget ->
              let
                ( newStyle, fx ) =
                  internalUpdate
                    --(normalizedDuration largestDuration (resolve action i))
                    (resolve action i)
                    (styleGet widget)
              in
                ( styleSet widget newStyle
                , Effects.map
                    (\a -> toInternalAction i (Internal a))
                    fx
                )
            )
            widgets
  in
    ( widgets, Effects.batch effects )



--normalizedDuration : Time -> InternalAction -> InternalAction
--normalizedDuration desiredDuration action =
--                            case action of
--                                Queue frames ->
--                                    Queue <| addBufferDuration frames desiredDuration
--                                Interrupt frames ->
--                                    Interrupt <| addBufferDuration frames desiredDuration
--                                _ -> action


{-| Adds a blank keyframe with a duration that makes the keyframes fill all the time until Time.

-}



--addBufferDuration : List StyleKeyframe -> Time -> List StyleKeyframe
--addBufferDuration frames desiredDuration =
--                let
--                  dur = getFullDuration frames
--                  delta = desiredDuration - dur
--                in
--                  if dur >= desiredDuration then
--                    frames
--                  else
--                    frames ++ [{ emptyKeyframe | duration = delta }]


{-|
-}



--getFullDuration : List StyleKeyframe -> Time
--getFullDuration frames =
--                    List.foldl
--                        (\frame total ->
--                            total + frame.delay + frame.duration
--                        )
--                        0 frames


{-| Specify the properties that should be animated

     UI.animate
         |> UI.duration (0.4*second)
         |> UI.props
             [ UI.Left UI.Px (UI.to 0)
             , UI.Opacity (UI.to 1)
             ]
         |> UI.on model.style

-}
props : List (StyleProperty (Physics DynamicTarget)) -> Action -> Action
props p action =
  updateOrCreate action (\a -> 
                            let
                                frame = a.frame
                                updatedFrame = { frame | target = p }
                            in
                                { a | frame = updatedFrame }
                        )


{-| Specify a duration.  If not specified, the default is 350ms.

   UI.animate
         |> UI.duration (0.4*second)
         |> UI.props
             [ UI.Left UI.Px (UI.to 0)
             , UI.Opacity (UI.to 1)
             ]
         |> UI.on model.style
-}
duration : Time -> Action -> Action
duration dur action =
    updateOrCreate action (\a -> { a | duration = Just dur })


{-| Specify a delay.  If not specified, the default is 0.

   UI.animate
         |> UI.duration (0.4*second)
         |> UI.delay (0.5*second)
         |> UI.props
             [ UI.Left UI.Px (UI.to 0)
             , UI.Opacity (UI.to 1)
             ]
         |> UI.on model.style
-}
delay : Time -> Action -> Action
delay delay action =
    updateOrCreate action (\a -> 
                            let
                                frame = a.frame
                                updatedFrame = { frame | delay = delay }
                            in
                                { a | frame = updatedFrame }
                        )

{-| Specify an easing function.  It is expected that values should match up at the beginning and end.  So, f 0 == 0 and f 1 == 1.  The default easing is sinusoidal
in-out.

-}
easing : (Float -> Float) -> Action -> Action
easing ease action =
    updateOrCreate action (\a -> { a | easing = Just ease })






{-| Animate based on spring physics.  You'll need to provide both a stiffness and a dampness to this function.


__Note:__ This will cause both `duration` and `easing` to be ignored as they are now controlled by the spring.

   UI.animate
         |> UI.spring UI.noWobble
         |> UI.props
             [ UI.Left UI.Px (UI.to 0)
             , UI.Opacity (UI.to 1)
             ]
         |> UI.on model.style
-}
spring : Spring.Properties -> Action -> Action
spring spring action =
  updateOrCreate action (\a -> { a | spring = Just spring })


{-| Append another keyframe.  This is used for multistage animations.  For example, to cycle through colors, we'd use the following:

      UI.animate
              |> UI.props
                  [ UI.BackgroundColor
                        UI.toRGBA 100 100 100 1.0
                  ]
          |> UI.andThen -- create a new keyframe
              |> UI.duration (1*second)
              |> UI.props
                  [ UI.BackgroundColor
                        UI.toRGBA 178 201 14 1.0
                  ]
          |> UI.andThen
              |> UI.props
                  [ UI.BackgroundColor
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
        Unstaggered <|
            { preaction | frames = preaction.frames ++ [ emptyKeyframeWithOptions ] }



{-| Update the last StyleKeyframe in the queue.  If the queue is empty, create a new StyleKeyframe and update that.
-}
updateOrCreate : Action -> (KeyframeWithOptions -> KeyframeWithOptions) -> Action
updateOrCreate action fn =
          case action of
            Internal ia ->
              Internal ia

            Staggered s ->
              Staggered s

            Unstaggered preaction ->
                Unstaggered <|
                  { preaction 
                        | frames = 
                                  case List.reverse preaction.frames of
                                    [] ->
                                      [ fn emptyKeyframeWithOptions ]

                                    cur :: rem ->
                                      List.reverse ((fn cur) :: rem)
                  }


{-| Animate a StyleProperty to a value.

-}
to : Float -> Physics DynamicTarget
to target =
  emptyPhysics <| 
    (\from current -> ((target - from) * current) + from)

{-| Animate a StyleProperty by adding to its existing value

-}
add : Float -> Physics DynamicTarget
add target =
  emptyPhysics <| 
    (\from current -> ((target - from) * current) + from)

{-| Animate a StyleProperty by subtracting to its existing value

-}
minus : Float -> Physics DynamicTarget
minus target =
  emptyPhysics <| 
    (\from current -> ((target - from) * current) + from)

{-| Keep an animation where it is!  This is useful for stacking transforms.

-}
stay : Float -> Physics DynamicTarget
stay target =
  emptyPhysics <| 
    (\from current -> from)



type alias ColorProperty =
  Physics DynamicTarget -> Physics DynamicTarget -> Physics DynamicTarget -> Physics DynamicTarget -> StyleProperty (Physics DynamicTarget)


{-| Animate a color-based property, given a color from the Color elm module.

-}
toColor : Color.Color -> ColorProperty -> StyleProperty (Physics DynamicTarget)
toColor color almostColor =
  let
    rgba =
      Color.toRgb color
  in
    almostColor
      (to <| toFloat rgba.red)
      (to <| toFloat rgba.green)
      (to <| toFloat rgba.blue)
      (to rgba.alpha)


{-| Animate a color-based style property to an rgb color.  Note: this leaves the alpha channel where it is.

     UI.animate
            |> UI.props
                [ UI.BackgroundColor
                      UI.toRGB 100 100 100
                ]
            |> UI.on model.style

-}
toRGB : Float -> Float -> Float -> ColorProperty -> StyleProperty (Physics DynamicTarget)
toRGB r g b prop =
  prop (to r) (to g) (to b) (to 1.0)


{-| Animate a color-based style property to an rgba color.

       UI.animate
            |> UI.props
                [ UI.BackgroundColor
                      UI.toRGBA 100 100 100 1.0
                ]
            |> UI.on model.style


-}
toRGBA : Float -> Float -> Float -> Float -> ColorProperty -> StyleProperty (Physics DynamicTarget)
toRGBA r g b a prop =
  prop (to r) (to g) (to b) (to a)


{-| Animate a color-based style property to an hsl color. Note: this leaves the alpha channel where it is.

-}
toHSL : Float -> Float -> Float -> ColorProperty -> StyleProperty (Physics DynamicTarget)
toHSL h s l prop =
  let
    rgba =
      Color.toRgb <| Color.hsl h s l
  in
    prop
      (to <| toFloat rgba.red)
      (to <| toFloat rgba.green)
      (to <| toFloat rgba.blue)
      (to rgba.alpha)


{-| Animate a color-based style property to an hsla color.

-}
toHSLA : Float -> Float -> Float -> Float -> ColorProperty -> StyleProperty (Physics DynamicTarget)
toHSLA h s l a prop =
  let
    rgba =
      Color.toRgb <| Color.hsl h s l
  in
    prop
      (to <| toFloat rgba.red)
      (to <| toFloat rgba.green)
      (to <| toFloat rgba.blue)
      (to rgba.alpha)


{-| Fade a color to a specific alpha level

-}



--fade : Float -> ColorProperty -> StyleProperty (Physics DynamicTarget)
--fade alpha prop =
--    prop stay stay stay (to alpha)


{-| Specify an initial Color-based property using a Color from the elm core Color module.

-}
fromColor : Color.Color -> (Static -> Static -> Static -> Static -> StyleProperty Static) -> StyleProperty Static
fromColor color almostColor =
  let
    rgba =
      Color.toRgb color
  in
    almostColor
      (toFloat rgba.red)
      (toFloat rgba.green)
      (toFloat rgba.blue)
      (rgba.alpha)


{-| Specify an initial Color-based property using rgb

-}
rgb : Float -> Float -> Float -> (Static -> Static -> Static -> Static -> StyleProperty Static) -> StyleProperty Static
rgb r g b prop =
  prop r g b 1.0


{-| Specify an initial Color-based property using rgba

-}
rgba : Float -> Float -> Float -> Float -> (Static -> Static -> Static -> Static -> StyleProperty Static) -> StyleProperty Static
rgba r g b a prop =
  prop r g b a


{-| Specify an initial Color-based property using hsl

-}
hsl : Float -> Float -> Float -> (Static -> Static -> Static -> Static -> StyleProperty Static) -> StyleProperty Static
hsl h s l prop =
  let
    rgba =
      Color.toRgb <| Color.hsl h s l
  in
    prop
      (toFloat rgba.red)
      (toFloat rgba.blue)
      (toFloat rgba.green)
      rgba.alpha


{-| Specify an initial Color-based property using hsla

-}
hsla : Float -> Float -> Float -> Float -> (Static -> Static -> Static -> Static -> StyleProperty Static) -> StyleProperty Static
hsla h s l a prop =
  let
    rgba =
      Color.toRgb <| Color.hsla h s l a
  in
    prop
      (toFloat rgba.red)
      (toFloat rgba.blue)
      (toFloat rgba.green)
      rgba.alpha



-- private
-- propCount refers to the how many times a property shows up
-- in the original list that prop is being pulled from


findProp : Style -> StyleProperty a -> Int -> Maybe (StyleProperty Static)
findProp state prop propCount =
  let
    findBy fn xs =
      List.head
        <| List.drop propCount
        <| List.filter fn
        <| xs

    matchPropID a b =
      Render.id a == Render.id b
  in
    findBy (matchPropID prop) state


{-| Render into concrete css that can be directly applied to 'style' in elm-html

    div [ style (UI.render widget.style) ] [ ]

-}
render : Animation -> List ( String, String )
render (A model) =
  let
    currentAnim =
      List.head model.anim
  in
    case currentAnim of
      Nothing ->
        let
          rendered =
            List.map renderProp model.previous

          transformsNprops =
            List.partition (\( name, _ ) -> name == "transform") rendered

          combinedTransforms =
            ( "transform"
            , String.concat
                (List.intersperse
                  " "
                  (List.map (snd) (fst transformsNprops))
                )
            )
        in
          snd transformsNprops ++ [ combinedTransforms ]

      Just anim ->
        -- Combine all transform properties
        let
          baked =
            bake anim model.previous

          rendered =
            List.map renderProp baked

          transformsNprops =
            List.partition (\s -> fst s == "transform") rendered

          combinedTransforms =
            ( "transform"
            , String.concat
                (List.intersperse
                  " "
                  (List.map (snd) (fst transformsNprops))
                )
            )
        in
          snd transformsNprops ++ [ combinedTransforms ]



-- private


renderProp : StyleProperty Static -> ( String, String )
renderProp prop =
  ( Render.name prop
  , Render.value prop
  )






-- private


fill : List (StyleProperty Static) -> List (StyleProperty Static) -> List (StyleProperty Static)
fill new existing =
  List.foldl
    (\x acc ->
      -- need to know the propIndex of x, meaning how many times it's shown up already.
      let
        xI =
          List.foldl
            (\x2 count ->
              if Render.id x == Render.id x2 then
                count + 1
              else
                count
            )
            0
            acc
      in
        case findProp new x xI of
          Nothing ->
            acc ++ [ x ]

          Just newX ->
            acc ++ [ newX ]
    )
    []
    existing



--bakeFinal : StyleKeyframe -> Style -> Style
--bakeFinal frame style = style


bake : StyleKeyframe -> Style -> Style
bake frame style =
  fill (List.map (mapProp toStatic) frame.target) style


toStatic : Physics DynamicTarget -> Static
toStatic physic =
  physic.position


mapProp : (a -> b) -> StyleProperty a -> StyleProperty b
mapProp fn prop =
  case prop of
    Prop n a u ->
      Prop n (fn a) u

    Opacity a ->
      Opacity (fn a)

    Height a u ->
      Height (fn a) u

    Width a u ->
      Width (fn a) u

    Left a u ->
      Left (fn a) u

    Top a u ->
      Top (fn a) u

    Right a u ->
      Right (fn a) u

    Bottom a u ->
      Bottom (fn a) u

    MaxHeight a u ->
      MaxHeight (fn a) u

    MaxWidth a u ->
      MaxWidth (fn a) u

    MinHeight a u ->
      MinHeight (fn a) u

    MinWidth a u ->
      MinWidth (fn a) u

    Padding a u ->
      Padding (fn a) u

    PaddingLeft a u ->
      PaddingLeft (fn a) u

    PaddingRight a u ->
      PaddingRight (fn a) u

    PaddingTop a u ->
      PaddingTop (fn a) u

    PaddingBottom a u ->
      PaddingBottom (fn a) u

    Margin a u ->
      Margin (fn a) u

    MarginLeft a u ->
      MarginLeft (fn a) u

    MarginRight a u ->
      MarginRight (fn a) u

    MarginTop a u ->
      MarginTop (fn a) u

    MarginBottom a u ->
      MarginBottom (fn a) u

    BorderWidth a u ->
      BorderWidth (fn a) u

    BorderRadius a u ->
      BorderRadius (fn a) u

    BorderTopLeftRadius a u ->
      BorderTopLeftRadius (fn a) u

    BorderTopRightRadius a u ->
      BorderTopRightRadius (fn a) u

    BorderBottomLeftRadius a u ->
      BorderBottomLeftRadius (fn a) u

    BorderBottomRightRadius a u ->
      BorderBottomRightRadius (fn a) u

    LetterSpacing a u ->
      LetterSpacing (fn a) u

    LineHeight a u ->
      LineHeight (fn a) u

    BackgroundPosition x y u ->
      BackgroundPosition (fn x) (fn y) u

    TransformOrigin x y z u ->
      TransformOrigin (fn x) (fn y) (fn z) u

    Color x y z a ->
      Color (fn x) (fn y) (fn z) (fn a)

    BackgroundColor x y z a ->
      BackgroundColor (fn x) (fn y) (fn z) (fn a)

    BorderColor x y z a ->
      BorderColor (fn x) (fn y) (fn z) (fn a)

    Translate a1 a2 u ->
      Translate (fn a1) (fn a2) u

    Translate3d a1 a2 a3 u ->
      Translate3d (fn a1) (fn a2) (fn a3) u

    TranslateX a u ->
      TranslateX (fn a) u

    TranslateY a u ->
      TranslateY (fn a) u

    Scale a ->
      Scale (fn a)

    Scale3d a1 a2 a3 ->
      Scale3d (fn a1) (fn a2) (fn a3)

    ScaleX a ->
      ScaleX (fn a)

    ScaleY a ->
      ScaleY (fn a)

    ScaleZ a ->
      ScaleZ (fn a)

    Rotate a u ->
      Rotate (fn a) u

    Rotate3d a1 a2 a3 a4 u ->
      Rotate3d (fn a1) (fn a2) (fn a3) (fn a4) u

    RotateX a u ->
      RotateX (fn a) u

    RotateY a u ->
      RotateY (fn a) u

    Skew a1 a2 u ->
      Skew (fn a1) (fn a2) u

    SkewX a u ->
      SkewX (fn a) u

    SkewY a u ->
      SkewY (fn a) u

    Perspective a ->
      Perspective (fn a)

    Matrix a b c x y z ->
      Matrix (fn a) (fn b) (fn c) (fn x) (fn y) (fn z)

    Matrix3d a b c d e f g h i j k l m n o p ->
      Matrix3d (fn a) (fn b) (fn c) (fn d) (fn e) (fn f) (fn g) (fn h) (fn i) (fn j) (fn k) (fn l) (fn m) (fn n) (fn o) (fn p)



-- Update


step : StyleKeyframe -> Style -> Time -> Time -> StyleKeyframe
step frame prev current dt =
  let
    style =
      List.foldl
        (\x acc ->
          -- need to know how many times x has shown up already.
          let
            xI =
              List.foldl
                (\x2 count ->
                  if Render.id x == Render.id x2 then
                    count + 1
                  else
                    count
                )
                0
                acc
          in
            case findProp prev x xI of
              Nothing ->
                acc

              Just prevX ->
                acc ++ [ stepProp x prevX current dt ]
        )
        []
        frame.target
  in
    { frame | target = style }


stepProp : StyleProperty (Physics DynamicTarget) -> StyleProperty Static -> Time -> Time -> StyleProperty (Physics DynamicTarget)
stepProp prop prev current dt =
  let
    val from physics =
      case physics.easing of
        Nothing ->
          let
            target =
              physics.target from 1.0

            newSpring =
              Spring.update dt physics.spring
          in
            { physics
              | spring = newSpring
              , position = ((target - from) * newSpring.position) + from
            }

        Just easing ->
          let
            eased =
              easing.ease (current / easing.duration)

            position =
              physics.target from eased
          in
            physics

  in
    case prop of
      Prop name to unit ->
        let
          from =
            case prev of
              Prop _ x _ ->
                x

              _ ->
                0.0
        in
          Prop name (val from to) unit

      Opacity to ->
        let
          from =
            case prev of
              Opacity x ->
                x

              _ ->
                0.0
        in
          Opacity (val from to)

      --Opacity ()
      Height to unit ->
        let
          from =
            case prev of
              Height x _ ->
                x

              _ ->
                0.0
        in
          Height (val from to) unit

      Width to unit ->
        let
          from =
            case prev of
              Width x _ ->
                x

              _ ->
                0.0
        in
          Width (val from to) unit

      Left to unit ->
        let
          from =
            case prev of
              Left x _ ->
                x

              _ ->
                0.0
        in
          Left (val from to) unit

      Top to unit ->
        let
          from =
            case prev of
              Top x _ ->
                x

              _ ->
                0.0
        in
          Top (val from to) unit

      Right to unit ->
        let
          from =
            case prev of
              Right x _ ->
                x

              _ ->
                0.0
        in
          Right (val from to) unit

      Bottom to unit ->
        let
          from =
            case prev of
              Bottom x _ ->
                x

              _ ->
                0.0
        in
          Bottom (val from to) unit

      MaxHeight to unit ->
        let
          from =
            case prev of
              MaxHeight x _ ->
                x

              _ ->
                0.0
        in
          MaxHeight (val from to) unit

      MaxWidth to unit ->
        let
          from =
            case prev of
              MaxWidth x _ ->
                x

              _ ->
                0.0
        in
          MaxWidth (val from to) unit

      MinHeight to unit ->
        let
          from =
            case prev of
              MinHeight x _ ->
                x

              _ ->
                0.0
        in
          MinHeight (val from to) unit

      MinWidth to unit ->
        let
          from =
            case prev of
              MinWidth x _ ->
                x

              _ ->
                0.0
        in
          MinWidth (val from to) unit

      Padding to unit ->
        let
          from =
            case prev of
              Padding x _ ->
                x

              _ ->
                0.0
        in
          Padding (val from to) unit

      PaddingLeft to unit ->
        let
          from =
            case prev of
              PaddingLeft x _ ->
                x

              _ ->
                0.0
        in
          PaddingLeft (val from to) unit

      PaddingRight to unit ->
        let
          from =
            case prev of
              PaddingRight x _ ->
                x

              _ ->
                0.0
        in
          PaddingRight (val from to) unit

      PaddingTop to unit ->
        let
          from =
            case prev of
              PaddingTop x _ ->
                x

              _ ->
                0.0
        in
          PaddingTop (val from to) unit

      PaddingBottom to unit ->
        let
          from =
            case prev of
              PaddingBottom x _ ->
                x

              _ ->
                0.0
        in
          PaddingBottom (val from to) unit

      Margin to unit ->
        let
          from =
            case prev of
              Margin x _ ->
                x

              _ ->
                0.0
        in
          Margin (val from to) unit

      MarginLeft to unit ->
        let
          from =
            case prev of
              MarginLeft x _ ->
                x

              _ ->
                0.0
        in
          MarginLeft (val from to) unit

      MarginRight to unit ->
        let
          from =
            case prev of
              MarginRight x _ ->
                x

              _ ->
                0.0
        in
          MarginRight (val from to) unit

      MarginTop to unit ->
        let
          from =
            case prev of
              MarginTop x _ ->
                x

              _ ->
                0.0
        in
          MarginTop (val from to) unit

      MarginBottom to unit ->
        let
          from =
            case prev of
              MarginBottom x _ ->
                x

              _ ->
                0.0
        in
          MarginBottom (val from to) unit

      BorderWidth to unit ->
        let
          from =
            case prev of
              BorderWidth x _ ->
                x

              _ ->
                0.0
        in
          BorderWidth (val from to) unit

      BorderRadius to unit ->
        let
          from =
            case prev of
              BorderRadius x _ ->
                x

              _ ->
                0.0
        in
          BorderRadius (val from to) unit

      BorderTopLeftRadius to unit ->
        let
          from =
            case prev of
              BorderTopLeftRadius x _ ->
                x

              _ ->
                0.0
        in
          BorderTopLeftRadius (val from to) unit

      BorderTopRightRadius to unit ->
        let
          from =
            case prev of
              BorderTopRightRadius x _ ->
                x

              _ ->
                0.0
        in
          BorderTopRightRadius (val from to) unit

      BorderBottomLeftRadius to unit ->
        let
          from =
            case prev of
              BorderBottomLeftRadius x _ ->
                x

              _ ->
                0.0
        in
          BorderBottomLeftRadius (val from to) unit

      BorderBottomRightRadius to unit ->
        let
          from =
            case prev of
              BorderBottomRightRadius x _ ->
                x

              _ ->
                0.0
        in
          BorderBottomRightRadius (val from to) unit

      LetterSpacing to unit ->
        let
          from =
            case prev of
              LetterSpacing x _ ->
                x

              _ ->
                0.0
        in
          LetterSpacing (val from to) unit

      LineHeight to unit ->
        let
          from =
            case prev of
              LineHeight x _ ->
                x

              _ ->
                0.0
        in
          LineHeight (val from to) unit

      BackgroundPosition x y unit ->
        case prev of
          BackgroundPosition xFrom yFrom _ ->
            BackgroundPosition (val xFrom x) (val yFrom y) unit

          _ ->
            BackgroundPosition (val 0.0 x) (val 0.0 y) unit

      Color x y z a ->
        let
          ( xFrom, yFrom, zFrom, aFrom ) =
            case prev of
              Color x1 y1 z1 a1 ->
                ( x1, y1, z1, a1 )

              _ ->
                ( 0.0, 0.0, 0.0, 0.0 )
        in
          Color (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a)

      BorderColor x y z a ->
        let
          ( xFrom, yFrom, zFrom, aFrom ) =
            case prev of
              BorderColor x1 y1 z1 a1 ->
                ( x1, y1, z1, a1 )

              _ ->
                ( 0.0, 0.0, 0.0, 0.0 )
        in
          BorderColor (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a)

      BackgroundColor x y z a ->
        let
          ( xFrom, yFrom, zFrom, aFrom ) =
            case prev of
              BackgroundColor x1 y1 z1 a1 ->
                ( x1, y1, z1, a1 )

              _ ->
                ( 0.0, 0.0, 0.0, 0.0 )
        in
          BackgroundColor (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a)

      TransformOrigin x y z unit ->
        let
          ( xFrom, yFrom, zFrom ) =
            case prev of
              TransformOrigin x1 y1 z1 _ ->
                ( x1, y1, z1 )

              _ ->
                ( 0.0, 0.0, 0.0 )
        in
          TransformOrigin (val xFrom x) (val yFrom y) (val zFrom z) unit

      Translate x y unit ->
        let
          ( xFrom, yFrom ) =
            case prev of
              Translate x1 y1 _ ->
                ( x1, y1 )

              _ ->
                ( 0.0, 0.0 )
        in
          Translate (val xFrom x) (val yFrom y) unit

      Translate3d x y z unit ->
        let
          ( xFrom, yFrom, zFrom ) =
            case prev of
              Translate3d x1 y1 z1 _ ->
                ( x1, y1, z1 )

              _ ->
                ( 0.0, 0.0, 0.0 )
        in
          Translate3d (val xFrom x) (val yFrom y) (val zFrom z) unit

      TranslateX to unit ->
        let
          from =
            case prev of
              TranslateX x _ ->
                x

              _ ->
                0.0
        in
          TranslateX (val from to) unit

      TranslateY to unit ->
        let
          from =
            case prev of
              TranslateY x _ ->
                x

              _ ->
                0.0
        in
          TranslateY (val from to) unit

      Scale to ->
        let
          from =
            case prev of
              Scale x ->
                x

              _ ->
                0.0
        in
          Scale (val from to)

      Scale3d x y z ->
        let
          ( xFrom, yFrom, zFrom ) =
            case prev of
              Scale3d x1 y1 z1 ->
                ( x1, y1, z1 )

              _ ->
                ( 0.0, 0.0, 0.0 )
        in
          Scale3d (val xFrom x) (val yFrom y) (val zFrom z)

      ScaleX to ->
        let
          from =
            case prev of
              ScaleX x ->
                x

              _ ->
                0.0
        in
          ScaleX (val from to)

      ScaleY to ->
        let
          from =
            case prev of
              ScaleY x ->
                x

              _ ->
                0.0
        in
          ScaleY (val from to)

      ScaleZ to ->
        let
          from =
            case prev of
              ScaleZ x ->
                x

              _ ->
                0.0
        in
          ScaleZ (val from to)

      Rotate to unit ->
        let
          from =
            case prev of
              Rotate x _ ->
                x

              _ ->
                0.0
        in
          Rotate (val from to) unit

      Rotate3d x y z a unit ->
        let
          ( xFrom, yFrom, zFrom, aFrom ) =
            case prev of
              Rotate3d x1 y1 z1 a1 _ ->
                ( x1, y1, z1, a1 )

              _ ->
                ( 0.0, 0.0, 0.0, 0.0 )
        in
          Rotate3d (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a) unit

      RotateX to unit ->
        let
          from =
            case prev of
              RotateX x _ ->
                x

              _ ->
                0.0
        in
          RotateX (val from to) unit

      RotateY to unit ->
        let
          from =
            case prev of
              RotateY x _ ->
                x

              _ ->
                0.0
        in
          RotateY (val from to) unit

      Skew x y unit ->
        let
          ( xFrom, yFrom ) =
            case prev of
              Skew x y _ ->
                ( x, y )

              _ ->
                ( 0.0, 0.0 )
        in
          Skew (val xFrom x) (val yFrom y) unit

      SkewX to unit ->
        let
          from =
            case prev of
              SkewX x _ ->
                x

              _ ->
                0.0
        in
          SkewX (val from to) unit

      SkewY to unit ->
        let
          from =
            case prev of
              SkewY x _ ->
                x

              _ ->
                0.0
        in
          SkewY (val from to) unit

      Perspective to ->
        let
          from =
            case prev of
              SkewY x _ ->
                x

              _ ->
                0.0
        in
          Perspective (val from to)

      Matrix a b c x y z ->
        case prev of
          Matrix aFrom bFrom cFrom xFrom yFrom zFrom ->
            Matrix
              (val aFrom a)
              (val bFrom b)
              (val cFrom c)
              (val xFrom x)
              (val yFrom y)
              (val zFrom z)

          _ ->
            Matrix
              (val 0.0 a)
              (val 0.0 b)
              (val 0.0 c)
              (val 0.0 x)
              (val 0.0 y)
              (val 0.0 z)

      Matrix3d a b c d e f g h i j k l m n o p ->
        case prev of
          Matrix3d a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 o2 p2 ->
            Matrix3d
              (val a2 a)
              (val b2 b)
              (val c2 c)
              (val d2 d)
              (val e2 e)
              (val f2 f)
              (val g2 g)
              (val h2 h)
              (val i2 i)
              (val j2 j)
              (val k2 k)
              (val l2 l)
              (val m2 m)
              (val n2 n)
              (val o2 o)
              (val p2 p)

          _ ->
            Matrix3d
              (val 0.0 a)
              (val 0.0 b)
              (val 0.0 c)
              (val 0.0 d)
              (val 0.0 e)
              (val 0.0 f)
              (val 0.0 g)
              (val 0.0 h)
              (val 0.0 i)
              (val 0.0 j)
              (val 0.0 k)
              (val 0.0 l)
              (val 0.0 m)
              (val 0.0 n)
              (val 0.0 o)
              (val 0.0 p)



{-| A spring preset.  Probably should be your initial goto for using springs.
-}
noWobble : Spring.Properties
noWobble =
  { stiffness = 170
  , damping = 26
  }


{-| A spring preset.
-}
gentle : Spring.Properties
gentle =
  { stiffness = 120
  , damping = 14
  }


{-| A spring preset.
-}
wobbly : Spring.Properties
wobbly =
  { stiffness = 180
  , damping = 12
  }


{-| A spring preset.
-}
stiff : Spring.Properties
stiff =
  { stiffness = 210
  , damping = 20
  }


{-| A spring preset.
-}
fastAndLoose : Spring.Properties
fastAndLoose =
  { stiffness = 320
  , damping = 17
  }


mapTo : Int -> (a -> a) -> List a -> List a
mapTo i fn xs =
  let
    update j x =
      if j == i then
        fn x
      else
        x
  in
    List.indexedMap update xs



