module Html.Animation (Animation, Action, init, update, render, animate, queue, stagger, on, props, delay, duration, easing, spring, andThen, set, forwardTo, forwardToIndex, forwardToAll, to, add, minus, stay, noWobble, gentle, wobbly, stiff, toColor, toRGB, toRGBA, toHSL, toHSLA, fromColor, rgb, rgba, hsl, hsla) where

{-| This library is for animating css properties and is meant to work well with elm-html.

The easiest way to get started with this library is to check out the examples that are included with the [source code](https://github.com/mdgriffith/elm-html-animation).

Once you have the basic structure of how to use this library, you can refer to this documentation to fill any gaps.


# Base Definitions
@docs Animation, Action

# Starting an animation
@docs animate, queue, stagger

# Creating animations
@docs props, delay, spring, duration, easing, andThen, set

# Animating Properties

These functions specify the value for a StyleProperty.

After taking an argument, these functions have `Float -> Float -> Float` as their signature.
This can be understood as `ExistingStyleValue -> CurrentTime -> NewStyleValue`, where CurrentTime is between 0 and 1.

@docs to, stay, add, minus

# Spring Presets
@docs noWobble, gentle, wobbly, stiff

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

# Managing Effects
@docs on, forwardTo, forwardToIndex, forwardToAll

-}

import Effects exposing (Effects)
import Time exposing (Time, second)
import String exposing (concat)
import List
import Color
import Html.Animation.Properties exposing (..)
import Html.Animation.Render as Render
import Html.Animation.Spring as Spring
import Html.Animation.Core as Core exposing (Static)


{-| An Animation of CSS properties.
-}
type Animation
  = A Core.Model


type alias KeyframeWithOptions =
  { frame : Core.StyleKeyframe
  , duration : Maybe Time
  , easing : Maybe (Float -> Float)
  , spring : Maybe Spring.Model
  }


{-| A Temporary type that is used in constructing actions.
-}
type alias PreAction =
  { frames : List KeyframeWithOptions
  , action : List Core.StyleKeyframe -> Core.Action
  }


type alias Dynamic =
  Core.Physics Core.DynamicTarget


{-| Actions to be run on an animation.
You won't be constructing this type directly, though it may show up in your type signatures.

To start animations you'll be using the `animate`, `queue`, and `stagger` functions
-}
type Action
  = Staggered (Float -> Float -> Action)
  | Unstaggered PreAction
  | Internal Core.Action


empty : Core.Model
empty =
  { elapsed = 0.0
  , start = Nothing
  , anim = []
  , previous = []
  , interruption = []
  }


emptyKeyframe : Core.StyleKeyframe
emptyKeyframe =
  { target = []
  , delay = 0.0
  }


emptyPhysics : a -> Core.Physics a
emptyPhysics target =
  { target = target
  , physical =
      { position = 0
      , velocity = 0
      }
  , spring =
      { stiffness = noWobble.stiffness
      , damping = noWobble.damping
      , destination = 1
      }
  , easing = Nothing
  }


emptyKeyframeWithOptions =
  { frame = emptyKeyframe
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
    newSpring = Just 
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


{-| Update an animation.  This will probably only show up once in your code.
See any of the examples at [https://github.com/mdgriffith/elm-html-animation](https://github.com/mdgriffith/elm-html-animation)
-}
update : Action -> Animation -> ( Animation, Effects Action )
update action (A model) =
  let
    ( newModel, fx ) =
      Core.update (resolve action 1 0) model
  in
    ( A newModel, Effects.map Internal fx )


{-| Begin describing an animation.  This animation will cleanly interrupt any animation that is currently running.

      UI.animate
         |> UI.duration (0.4*second)
         |> UI.props
             [ Left (UI.to 0) Px
             , Opacity (UI.to 1)
             ]
         |> UI.on model.style

-}
animate : Action
animate =
  Unstaggered
    <| { frames = []
       , action = Core.Interrupt
       }


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




{-| Apply an update to a Animation model.  This is used at the end of constructing an animation.
However, you'll have an overall cleaner syntax if you use `forwardTo` to prepare a custom version of `on`.

     UI.animate
         |> UI.duration (0.4*second)
         |> UI.props
             [ Left (UI.to 0) Px 
             , Opacity (UI.to 1)
             ]
         |> UI.on model.style

-}
on : Animation -> Action -> ( Animation, Effects Action )
on model action =
  update action model


{-| Resolve the stagger if there is one, and apply springs if present.

-}
resolve : Action -> Int -> Int -> Core.Action
resolve stag t i =
  case stag of
    Unstaggered preaction ->
      preaction.action
        <| List.map
            applyKeyframeOptions
            preaction.frames

    Staggered s ->
      resolve (s (toFloat t) (toFloat i)) t i

    Internal ia ->
      ia


applyKeyframeOptions : KeyframeWithOptions -> Core.StyleKeyframe
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
        Core.mapProp addOptions prop
  in
    { frame | target = List.map applyOpt frame.target }









{-|  This function is used to handle the boilerplate of forwarding animation updates, and can be used in place of `on`.

To use this function, you'll need to supply a getter and a setter function for getting and setting the style model.

So, for a model like the following

    type alias Model = { style : UI.Animation }

Add an action to your Action type to capture the UI.Actions.

    type Action 
          = Hide
          | Animate UI.Action


Create a specialized version of `forwardTo`.

    onModel = 
      UI.forwardTo
          Animate -- The action that captures UI.Action 
          .style -- style getter
          (\w style -> { w | style = style }) -- style setter

Then, in your update function would look something like 


      Hide ->
        UI.animate
            |> UI.duration (5*second)
            |> UI.props
                [ Opacity (UI.to 0)
                ]
            |> onModel model

      Animate uiAction ->
        onModel model uiAction


-}
forwardTo : (Action -> b) -> (a -> Animation) -> (a -> Animation -> a) -> a -> Action -> ( a, Effects b )
forwardTo toInternalAction styleGet styleSet widget action =
    let
      ( A anim ) =
        styleGet widget

      ( newStyle, fx ) =
        Core.update (resolve action 1 0) anim

    in
      ( styleSet widget (A newStyle)
      , Effects.map
          (\a -> toInternalAction (Internal a))
          fx
      )


{-| Forward style updates to a specific element in a list that has a Animation model.

For a model like the following

    type alias Model = { widgets : List Widget }

    type alias Widget =
              { style : UI.Animation
              }
  
    type Action 
          = Hide
          | Animate Int UI.Action -- where Int is the index of the widget we are animating

Create a specialized version of `forwardToIndex`.

    onWidget = 
      UI.forwardToIndex
          Animate
          .style -- widget style getter
          (\w style -> { w | style = style }) -- widget style setter

And in your update function:

    Hide ->
      let
        (widgets, fx) =
            UI.animate
                |> UI.duration (5*second)
                |> UI.props
                    [ Opacity (UI.to 0)
                    ]
                |> onWidget i model.widgets
                -- Where i is the index of the widget to update.
      in
        ( { model | widgets = widgets }
        , fx ) -- FX has already been `Effects.map`ped to Animate

    Animate i action ->
      let
        (widgets, fx) = 
            onWidget i model.widgets action
      in
        ( { model | widgets = widgets }
        , fx )


-}
forwardToIndex : (Int -> Action -> b) -> (a -> Animation) -> (a -> Animation -> a) -> Int -> List a -> Action -> ( List a, Effects b )
forwardToIndex toInternalAction styleGet styleSet i widgets action =
  let
    numWidgets =
      List.length widgets

    ( widgets, effects ) =
      List.unzip
        <| List.indexedMap
            (\j widget ->
              if j == i then
                let
                  (A anim) =
                    styleGet widget

                  ( newStyle, fx ) =
                    Core.update
                      (resolve action numWidgets i)
                      anim
                in
                  ( styleSet widget (A newStyle)
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


{-| Like `forwardToIndex`, except it applies an update to every member of the list.
It has the same set up as `forwardToIndex`, except:

You'll need two helper functions

  onWidget = 
    UI.forwardToIndex
        Animate
        .style -- widget style getter
        (\w style -> { w | style = style }) -- widget style setter
                                      
  onAllWidgets = 
      UI.forwardToAll 
          Animate
          .style -- widget style getter
          (\w style -> { w | style = style }) -- widget style setter



And your update function will look like the following

    Hide ->
      let 
        (widgets, fx) = 
            UI.animate
               |> UI.delay ((i * 0.05) * second)
               |> UI.spring UI.wobbly
               |> UI.props 
                   [ Left (UI.to -70) Px
                   ] 
              |> onAllWidgets model.widgets 
              -- apply an update to all widgets

      in
        ( { model | widgets = widgets }
        , fx )

    
    -- But, in animate, you only need to forwad to a widget based on index
    Animate i action ->
      let
        (widgets, fx) = 
            onWidget i model.widgets action
      in
        ( { model | widgets = widgets }
        , fx )




-}
forwardToAll : (Int -> Action -> b) -> (a -> Animation) -> (a -> Animation -> a) -> List a -> Action -> ( List a, Effects b )
forwardToAll toInternalAction styleGet styleSet widgets action =
  let
    numWidgets =
      List.length widgets

    ( widgets, effects ) =
      List.unzip
        <| List.indexedMap
            (\i widget ->
              let
                (A anim) =
                  styleGet widget

                ( newStyle, fx ) =
                  Core.update
                    (resolve action numWidgets i)
                    anim
              in
                ( styleSet widget (A newStyle)
                , Effects.map
                    (\a -> toInternalAction i (Internal a))
                    fx
                )
            )
            widgets
  in
    ( widgets, Effects.batch effects )


{-| Specify the properties that should be animated

     UI.animate
         |> UI.duration (0.4*second)
         |> UI.props
             [ Left (UI.to 0) Px
             , Opacity (UI.to 1)
             ]
         |> UI.on model.style

-}
props : List (StyleProperty Dynamic) -> Action -> Action
props p action =
  updateOrCreate
    action
    (\a ->
      let
        frame =
          a.frame

        updatedFrame =
          { frame | target = p }
      in
        { a | frame = updatedFrame }
    )



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
set : List (StyleProperty Static) -> Action -> Action
set staticProps action = 
          let
            dynamic = List.map 
                        (Core.mapProp (\x -> to x)) 
                        staticProps
          in
            updateOrCreate
              action
              (\a ->
                let
                  frame =
                    a.frame

                  updatedFrame =
                    { frame 
                        | target = dynamic
                    }
                in
                  { a | frame = updatedFrame
                      , duration = Just 0
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
  updateOrCreate
    action
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


{-| Update the last Core.StyleKeyframe in the queue.  If the queue is empty, create a new Core.StyleKeyframe and update that.
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


{-| Animate a StyleProperty to a value.
-}
to : Float -> Dynamic
to target =
  emptyPhysics
    <| (\from current -> ((target - from) * current) + from)


{-| Animate a StyleProperty by adding to its existing value
-}
add : Float -> Dynamic
add mod =
  emptyPhysics
    <| (\from current -> 
            let
              target = from + mod
            in
              ((target - from) * current) + from
        )


{-| Animate a StyleProperty by subtracting to its existing value
-}
minus : Float -> Dynamic
minus mod =
   emptyPhysics
    <| (\from current -> 
            let
              target = from - mod
            in
              ((target - from) * current) + from
        )


{-| Keep an animation where it is!  This is useful for stacking transforms.
-}
stay : Dynamic
stay =
  emptyPhysics
    <| (\from current -> from)



type alias ColorProperty =
  Dynamic -> Dynamic -> Dynamic -> Dynamic -> StyleProperty Dynamic


{-| Animate a color-based property, given a color from the Color elm module.

-}
toColor : Color.Color -> ColorProperty -> StyleProperty Dynamic
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
                [ BackgroundColor
                      UI.toRGB 100 100 100
                ]
            |> UI.on model.style

-}
toRGB : Float -> Float -> Float -> ColorProperty -> StyleProperty Dynamic
toRGB r g b prop =
  prop (to r) (to g) (to b) (to 1.0)


{-| Animate a color-based style property to an rgba color.

       UI.animate
            |> UI.props
                [ BackgroundColor
                    UI.toRGBA 100 100 100 1.0
                ]
            |> UI.on model.style


-}
toRGBA : Float -> Float -> Float -> Float -> ColorProperty -> StyleProperty Dynamic
toRGBA r g b a prop =
  prop (to r) (to g) (to b) (to a)


{-| Animate a color-based style property to an hsl color. Note: this leaves the alpha channel where it is.

-}
toHSL : Float -> Float -> Float -> ColorProperty -> StyleProperty Dynamic
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
toHSLA : Float -> Float -> Float -> Float -> ColorProperty -> StyleProperty Dynamic
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


{-| Render into concrete css that can be directly applied to 'style' in elm-html

    div [ style (UI.render widget.style) ] [ ]

-}
render : Animation -> List ( String, String )
render (A model) =
    case List.head model.anim of
      Nothing ->
        Render.render model.previous
        
      Just anim ->
        Render.render <| Core.bake anim model.previous
       
