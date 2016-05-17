
# elm-style-animation

A library to simplify creating html and svg animations in elm. My focus was to create something I could use as a UI designer to prototype animations quickly, accurately, and without sneaky errors.  

  1. Showing a menu on hover - [demo](https://mdgriffith.github.io/elm-style-animation/1.0.0/SideMenu.html) / [view code](https://github.com/mdgriffith/elm-style-animation/blob/master/examples/SideMenu.elm)
  2. Chaining Keyframes - [demo](https://mdgriffith.github.io/elm-style-animation/1.0.0/Chaining.html) / [view code](https://github.com/mdgriffith/elm-style-animation/blob/master/examples/Chaining.elm)
  3. Updating based on Current Style.
  4. Animating a List of Elements - [demo](https://mdgriffith.github.io/elm-style-animation/1.0.0/Showcase.html) / [view code](https://github.com/mdgriffith/elm-style-animation/blob/master/examples/Showcase.elm)
      * Staggering animations - [demo](https://mdgriffith.github.io/elm-style-animation/1.0.0/Stagger.html) / [view code](https://github.com/mdgriffith/elm-style-animation/blob/master/examples/Stagger.elm)
  5. Stacking transformations for complex animations - [demo](https://mdgriffith.github.io/elm-style-animation/1.0.0/StackingTransforms.html) / [view code](https://github.com/mdgriffith/elm-style-animation/blob/master/examples/StackingTransforms.elm)
  6. Animating SVG
      * Morphing Shapes - Elm Logo [demo](https://mdgriffith.github.io/elm-style-animation/1.0.0/Logo.html) / [view code](https://github.com/mdgriffith/elm-style-animation/blob/master/examples/Logo.elm)
      * Morphing Batman Logos - [inspiration](http://tavmjong.free.fr/blog/?p=741) / [demo](https://mdgriffith.github.io/elm-style-animation/1.0.0/Batman.html) / [view code](https://github.com/mdgriffith/elm-style-animation/blob/master/examples/Batman.elm)
  7. Realistic scenario (flower menu) (separate repo) - [demo](https://mdgriffith.github.io/elm-style-animation/1.0.0/FlowerMenu/) / [view code](https://github.com/mdgriffith/elm-html-animation-flower-menu/blob/master/FlowerMenu.elm)


## Installation

First have [Elm installed](http://elm-lang.org/install), then

If you just want to play with the examples, run the following in a console:

```bash
git clone https://github.com/mdgriffith/elm-style-animation.git
cd elm-style-animation/examples
elm-reactor
# Make sure to cd into the examples folder.
# The library and the examples have different dependencies
# So, running elm-reactor on the base folder will not work
# if you just want to run examples.
```

Or, if you want to install the package in one of your elm projects.

```bash
elm-package install mdgriffith/elm-style-animation
```


## The Basics

I recommend checking out [the Elm Architecture](https://github.com/evancz/elm-architecture-tutorial/) if you haven't already.  These examples will be much easier if you're already familiar with Elm in general and the standard `model`, `update`, `view` pattern.

So, with all that in mind, here's a basic overview of what you'll need to do to use this library.

To add animations to a module, you'll need to do the following:

  * Store the styling data in your `model`, and define an initial style.
  * Subscribe to the browser's animation frame

__Note__ all properties that are going to be animated need to be accounted for in the initial style.

```elm

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Color exposing (rgba)
import AnimationFrame
import Time exposing (Time)
import Style
import Style.Properties exposing (..)

type alias Model = { style : Style.Animation }

init : Model
init = { style =
            Style.init
                [ Left -350.0 Px
                , Opacity 0.0
                , Color (rgba 50 50 50 1.0)
                ]
        }

-- Create a subscription to the browser's animation frame
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

```


 * In your `view`, render the animation as a css style.

```elm
view : Model -> Html Msg
view model =
      div [ style (Style.render model.style) ] []

```


  * Add a new action to your Action type to allow updates to be sent to an animation.

```elm
type Msg = Show -- This message triggers the animation
         | Animate Time -- This message forwards all updates to the elm-style-animation core.

-- In our update function, we can use the helper function trigger animations
-- and to pass updates to an animation
update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case action of

    Show ->
      let
          style =
              Style.animate
                 |> Style.to [ Opacity 1]
                 |> Style.on model.style
      in
        ( { model | style = style}
        , Cmd.none
        )

     -- for each animation frame, update the style.
     -- If we have multiple styles to manage, they will need to be updated in this action.
     Animate time ->
        ( { model
              | style = Style.tick time model.style
          }
        , Cmd.none
        )
```


  * Now that we're set up, we can begin animating.


# Example 1: Showing a Menu on Hover

[demo](https://mdgriffith.github.io/elm-style-animation/1.0.0/SideMenu.html) / [view code](https://github.com/mdgriffith/elm-style-animation/blob/master/examples/SideMenu.elm)

Our first example is a menu that is shown when the mouse enters a certain area, and hides when the mouse leaves.

So, our first step is to add two values to our Action type, `Show` and `Hide`, and in the update function, we start an animation when those actions occur.  Let's take a look at how we construct an animation.


```elm
-- ... we add this to the case statement in our update function.

    Style.animate
    -- |> Style.delay (0.5*second)
       |> Style.to
           [ Left 0 Px
           , Opacity 1
           ]
       |> Style.on model.style


```

Notice we are programming declaratively by defining what style property should be by using `Style.to`.  A delay is present but commented out.

Instead of using a duration and an easing function, this library defaults to animating by using a __spring__.  By modeling real-world springs, we can create organic animations by defining two numbers, _stiffness_ and _damping_.


```elm
-- ... we add this to the case statement in our update function.
    Style.animate
    -- |> Style.spring Style.Spring.Preset.wobbly -- you can use a UI preset
    -- or specify manually.
       |> Style.spring
            { stiffness = 400
            , damping = 28
            }
       |> Style.to
           [ Left 0 Px
           , Opacity 1
           ]
       |> Style.on model.style

```


> Alternatively, we also have the option of defining a _duration_, and an _easing function_.  I've generally found that springs are a more natural way for animating user interfaces, but there are some cases where easing and duration could be preferable.  Here's how it's done.

 ```elm
       Style.animate
           |> Style.easing (\x -> x)  -- linear easing
           |> Style.duration (0.5*second)
           |> Style.to
               [ Left 0 Px
               , Opacity 1
               ]
           |> Style.on model.style
  ```
>
> __Note__ The duration you provide will not do anything unless you also provide an easing function.  This is because spring based animations set the duration dynamically.
> Make sure to check out the [elm community easing library](http://package.elm-lang.org/packages/elm-community/easing-functions/latest) if you're looking for easing functions.

Now that we have this animation, it has a few properties that may not be immediately apparent.  If a `Hide` action is called halfway through execution of the `Show` animation, the animation will be smoothly interrupted.

There may be a situation where we don't want our animation to be interrupted and instead we want an animation to queue up after a currently running animation.  To do this, we would use `Style.queue` instead of `Style.animate`


# Example 2: Chaining Keyframes

[demo](https://mdgriffith.github.io/elm-style-animation/1.0.0/Chaining.html) / [view code](https://github.com/mdgriffith/elm-style-animation/blob/master/examples/Chaining.elm)

What we've been doing is creating a single keyframe animation, but we also have the option of adding more keyframes.  

We use `Style.andThen` to create a new keyframe.  This new keyframe will have it's own independent delay, properties, and spring (or easing + duration).  Again, it can be interrupted smoothely at any point.


```elm
-- we need to import Color to begin working with color.
import Color exposing (rgba)

-- in our update function, we'd change our animation to:
      Style.animate
        |> Style.to
            [ BackgroundColor (rgba 100 100 100 1.0) ]
        |> Style.andThen -- create a new keyframe
        |> Style.to
            [ BackgroundColor (rgba 178 201 14 1.0) ]
        |> Style.andThen
        |> Style.to
            [ BackgroundColor (rgba 58 40 69 1.0) ]
        |> on model.menuStyle

```




# Example 3: Animating Lists of Elements

[demo](https://mdgriffith.github.io/elm-style-animation/1.0.0/Showcase.html) / [view code](https://github.com/mdgriffith/elm-style-animation/blob/master/examples/Showcase.elm)

We can animate a list of styles by updating the styles either with `List.map` or `List.indexedMap`.

First, our model would be something like this:

```elm

type alias Model = { widgets : List Style.Animation }

-- Later, in our update statement...
      -- where j is the index of the widget we want to animate.
      let
        widgets =
            List.indexedMap
              (\i widget ->
                  -- only update a specific widget in a list.
                  if i == j then
                     Style.animate
                        |> Style.duration (5*second)
                        |> Style.to
                            [ Opacity 0  
                            ]
                        |> Style.on widget
                  else
                    widget
              ) model.widgets


      in
        ( { model | widgets = widgets }
        , Cmd.none )

-- Later in the `Animate` section of our `update` function, we need to send updates to every style we're animating.

    Animate time ->
        ( { model
            | widgets =
                List.map
                    (\widget ->
                        Style.tick time widget
                    )
                    model.widgets
          }
        , Cmd.none
        )


```

By using `List.map` and `List.indexedMap` we have a natural way to do things like staggering a series of animations.  We can just calculate the delay of an animation based on it's index in a list.

Staggering animations - [demo](https://mdgriffith.github.io/elm-style-animation/1.0.0/Stagger.html) / [view code](https://github.com/mdgriffith/elm-style-animation/blob/master/examples/Stagger.elm)

```elm
    List.indexedMap
        (\i widget ->
           Style.animate
             |> Style.delay (i * 0.05 * second) -- stagger this animation.
             |> Style.duration (0.3 * second)
             |> Style.to
                 [ Left 200 Px
                 ]
             |> Style.on widget
        ) model.widgets

```



# Example 4: Stacking Transformations

[demo](https://mdgriffith.github.io/elm-style-animation/1.0.0/StackingTransforms.html) / [view code](https://github.com/mdgriffith/elm-style-animation/blob/master/examples/StackingTransforms.elm)

CSS has support for `transforms` such as `translate`, `rotate`, and `scale`.  We also have access to some more complicated transformations such as `rotate3d` and `transform3d`.

When using these transformations in normal css, you're able to stack them.  So, in your css file you can have something like the following:

```css
.transformed {
  transform: rotate(20deg) translateY(100px) rotate(-20deg)
}
```

In this case, the transforms are performed in order. Rotate by 20deg, translateY (which is now angled 20deg) by 100px, and then rotate again -20deg.

This can be very useful, especially if we can animate each transform element individually.

Here's how we're able to do this in Html.Animation.

First, we define our initial style.  This will define the order that the transforms will appear.

```elm
initialWidgetStyle =
        Style.init
            [ Rotate 0 Deg
            , TranslateY 0 Px
            , Rotate 0 Deg
            ]

```


Now let's animate these properties.  Let's say we want do the following animation:

  1. Make the first `Style.Rotate` move to 20deg.  
  2. Once that's finished, we want to translateY to -200px
  3. Now we want to rotate 1 revolution locally
  4. Now we want to rotate 1 revolution around the original center
  5. Then once that's finished, reset everything to 0.

```elm
  Style.animate
      |> Style.duration (0.5*second)
      |> Style.to
          [ Rotate 20 Deg
          ]
      |> Style.andThen
      |> Style.duration (0.7*second)
      |> Style.props
          [ TranslateY -200 Px
          ]
      |> Style.andThen
      |> Style.duration (0.7*second)
      |> Style.update
          (\index prop ->
              case prop of
                  Rotate angle unit ->
                     -- make this update apply to the second rotate only.
                     if index == 2 then
                        Rotate 360.0 unit
                    else
                        Rotate angle unit
                  _ -> prop
          )

      |> Style.andThen
      |> Style.duration (0.7*second)
      |> Style.to
          [ Rotate 380 Deg
          ]
      |> Style.andThen
      |> Style.delay (1*second)
      |> Style.to
          [ Rotate 0 Deg
          , TranslateY 0 Px
          , Rotate 0 Deg
          ]

```

## Animating SVG

Animating svg has to be handled slightly differently than animating html because the majority of the interesting properties that we'd want to animate are actually attributes that can't be controlled by CSS.

However there's an easy solution.  Just use `Style.renderAttr` instead of `Style.render`, and everything will be take care of.  For example:

```
import Color exposing (blue, green)
import Style
import Style.Properties exposing (..)

-- Style.Propeties also exposes svg properties
model {
    svgStyle = Style.init
          [ Fill blue
          , Cx 200
          , Cy 300
          , R 50
          ]
}

...in your view function, use Style.renderAttr like so.

    svg []
        [ circle (Style.renderAttr model.svgStyle) []
        ]

```

Everything else that you learned applies exactly the same to svg properties.  Only the render function changes.

Here are the properties you can use in svg animations.
   * X
   * Y
   * Cx
   * Cy
   * R
   * Rx
   * Ry
   * D
   * Points
   * Fill
   * Stroke



## Morphing Shapes
Elm Logo [demo](https://mdgriffith.github.io/elm-style-animation/1.0.0/Logo.html) / [view code](https://github.com/mdgriffith/elm-style-animation/blob/master/examples/Logo.elm)

If you create an svg polygon you can animate using the `points` attribute.  Elm-style-animation will automatically convert between polygons with differing numbers of points.  

```elm
-- We can define two polygon styles and morph between them

      [ Points
              <| alignStartingPoint
                  [ ( 161.649, 152.782 )
                  , ( 231.514, 82.916 )
                  , ( 91.783, 82.916 )
                  ]
        , Fill palette.orange
        ]
      , [ Points
              <| alignStartingPoint
                  [ ( 8.867, 0 )
                  , ( 79.241, 70.375 )
                  , ( 232.213, 70.375 )
                  , ( 161.838, 0 )
                  ]
        , Fill palette.green
        ]



```

To smoothly morph between two polygons, we need to align the starting points.  Fortunately we can do that with `alignStartingPoint`, which rotates a list of coordinates to that the one closest to the origin comes first and the rest follow.  


## Morphing Paths - Batman Logos
[inspiration](http://tavmjong.free.fr/blog/?p=741) / [demo](https://mdgriffith.github.io/elm-style-animation/1.0.0/Batman.html) / [view code](https://github.com/mdgriffith/elm-style-animation/blob/master/examples/Batman.elm)

You can also morph between svg [paths](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d) using the `d` property.  Unlike the points property we were just talking about, we can't animate between two paths unless they have the same number of path commands.

Paths are defined using the following.
```elm
model = {
  myPath = Style.init
      [ D [ MoveTo 256 213
          , CurveTo [(245,181), (206,187), (234,262), (147,181), (169,71.2), (233,18)]
          , Close
          ]
      ]      
}
```

Check out the [batman morphing example](https://github.com/mdgriffith/elm-style-animation/blob/master/examples/Batman.elm) to dive in.
