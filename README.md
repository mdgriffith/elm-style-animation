
# elm-html-animation

A library to simplify creating html animations in elm. My focus was to create something I could use as a UI designer to prototype animations quickly, accurately, and without sneaky errors.

  1. Showing a menu on hover - [demo](https://mdgriffith.github.io/elm-html-animation/examples/SideMenu.html) / [view code](https://github.com/mdgriffith/elm-html-animation/blob/master/examples/SideMenu.elm)
  2. Chaining animations together - [demo](https://mdgriffith.github.io/elm-html-animation/examples/Chaining.html) / [view code](https://github.com/mdgriffith/elm-html-animation/blob/master/examples/Chaining.elm)
  3. Managing multiple animations - [demo](https://mdgriffith.github.io/elm-html-animation/examples/Showcase.html) / [view code](https://github.com/mdgriffith/elm-html-animation/blob/master/examples/Showcase.elm)
  4. Stacking transformations - [demo](https://mdgriffith.github.io/elm-html-animation/examples/StackingTransforms.html) / [view code](https://github.com/mdgriffith/elm-html-animation/blob/master/examples/StackingTransforms.elm)
  5. Staggering animations - [demo](https://mdgriffith.github.io/elm-html-animation/examples/Stagger.html) / [view code](https://github.com/mdgriffith/elm-html-animation/blob/master/examples/Stagger.elm)


## Installation

Try it out!

First have [Elm installed](http://elm-lang.org/install), then

If you just want to play with the examples, run the following in a console:

```bash
git clone https://github.com/mdgriffith/elm-html-animation.git
cd elm-html-animation/examples
elm-reactor
# Make sure to cd into the examples folder.
# The library and the examples have different dependencies
# So, running elm-reactor on the base folder will not work
# if you just want to run examples.
```

Or, if you want to install the package in one of your elm projects.

```bash
elm-package install mdgriffith/elm-html-animation
```


## The Basics

I recommend checking out [the Elm Architecture](https://github.com/evancz/elm-architecture-tutorial/) if you haven't already.  These examples will be much easier if you're already familiar with the standard model, update, and view pattern.  As well you should understand the basics of using the `Effects` library.

So, with all that in mind, here's a basic overview of what you'll need to do to use this library.

To add animations to a module, you'll need to do the following:

  * Store the styling data in your `model`, and define an initial style.

__Note__ all properties that are going to be animated need to be accounted for in the initial style.

```elm
import Html.Animation as UI

type alias Model = { style : UI.Animation }

init : Model
init = { style = 
            UI.init 
                [ UI.Left -350.0 UI.Px
                , UI.Opacity 0.0 
                , UI.Color |> rgba 50 50 50 1.0
                ]
        }
```

  * Add a new action to your Action type to allow updates to be sent to an animation.

```elm
type Action = Animate UI.Action

-- and in our update function, we need the following code to allow for updates to be passed to an animation

update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of

    Animate action ->
      let
        (anim, fx) = UI.update action model.style
      in
        ( { model | style = anim }
        , Effects.map Animate fx )
```

  * In your `view`, render the animation as a css style.

```elm

view : Address Action -> Model -> Html
view address model =
          div [ style (UI.render model.style) ] []

```


  * Start creating animations!  Now that we're set up, we can begin animating. 


# Example 1: Showing a Menu on Hover

[demo](https://mdgriffith.github.io/elm-html-animation/examples/SideMenu.html) / [view code](https://github.com/mdgriffith/elm-html-animation/blob/master/examples/SideMenu.elm)

Our first example is a menu that is shown when the mouse enters a certain area, and hides when the mouse leaves.

So, our first step is to add two values to our Action type, `Show` and `Hide`, and in the update function, we start an animation when those actions occur.  Let's take a look at how we construct an animation.


```elm
-- ... we add this to the case statement in our update function.
    Show ->
      let 
        (anim, fx) = 
              UI.animate 
              -- |> UI.duration (0.5*second)
              -- |> UI.delay (0.5*second)
              -- |> UI.easing (\x -> x)
                 |> UI.props 
                     [ UI.Left (UI.to 0) UI.Px
                     , UI.Opacity (UI.to 1)
                     ] 
                 |> UI.on model.style

      in
        ( { model | style = anim }
        , Effects.map Animate fx )
```

Notice we are programming declaratively by defining what style property should be by using `UI.to`.  

We also have the option of defining a _duration_, a _delay_, and an _easing function_. 

In the above code, these are commented out, which means the defaults are used.  

| Option   | Default |
|----------|---------|
| duration | 350ms   |
| delay    | 0       |
| easing   | _sinusoidal in-out_ |

Make sure to check out this [library](http://package.elm-lang.org/packages/Dandandan/Easing/2.0.1/Easing#easing-functions) if you're looking for easing functions.

Finally, we have to `map` the resulting animation Effect, `fx`, to the Animate action we created earlier.

Now that we have this animation, it has a few properties that may not be immediately apparent.  If a `Hide` action is called halfway through execution of the `Show` animation, the animation will be smoothly interrupted. 

However, there may be a situation where we don't want our animation to be interrupted.  Instead we might want the current animation to play out completely and for our new animation to play directly afterwards.  To do this, we would use `UI.queue` instead of `UI.animate`

> ### Spring-based Animation
> Instead of using a duration and an easing function, you also have the option of animating by using a __spring__.  Using the math that models real-world springs, we can create organic animations by just defining two numbers, _stiffness_ and _damping_.  Here's an example of using a spring:
>
  ```elm
    (anim, fx) = 
           UI.animate 
               |> UI.spring { stiffness = 180
                            , damping = 12
                            }
               |> UI.props 
                   [ UI.Left (UI.to 0) UI.Px
                   , UI.Opacity (UI.to 1)
                   ] 
               |> UI.on model.style
  ```
>  __Note:__ If you provide a spring, it will override any duration or easing you may have specified. 
>
> A number of spring presets are provided in the library.



# Example 2: Chaining Animations Together

[demo](https://mdgriffith.github.io/elm-html-animation/examples/Chaining.html) / [view code](https://github.com/mdgriffith/elm-html-animation/blob/master/examples/Chaining.elm)

We also have option of chaining animations together.  So, let's make a square that cycles through a few background colors when you click it.

```elm
-- in our update function:
    ChangeColor ->
      let 
        (anim, fx) = 
            UI.animate 
                    |> UI.props 
                        [ UI.BackgroundColor 
                              UI.toRgba 100 100 100 1.0
                        ] 
                |> UI.andThen -- create a new keyframe
                    |> UI.duration (1*second)
                    |> UI.props 
                        [ UI.BackgroundColor 
                              UI.toRgba 178 201 14 1.0
                        ] 
                |> UI.andThen 
                    |> UI.props 
                        [ UI.BackgroundColor 
                              UI.toRgba 58 40 69 1.0 
                        ] 
                |> UI.on model.style 

      in
        ( { model | style = anim }
        , Effects.map Animate fx )

```


In this case we can use `UI.andThen` to create a new key frame.  This new keyframe will have it's own independent duration, easing, and properties.  Again, it can be interrupted at any point.


# Example 4: Managing Multiple Animations

[demo](https://mdgriffith.github.io/elm-html-animation/examples/Showcase.html) / [view code](https://github.com/mdgriffith/elm-html-animation/blob/master/examples/Showcase.elm)

It's also fairly common to have a list of elements that all need to animated individually.  In order to do this we need to make a few changes to our boilerplate code we had in the beginning.


First, our model would be something like this:

```elm

type alias Model = { widgets : List Widget }

type alias Widget = 
          { style : UI.Animation
          }

```


We also need to prepare a function that will perform an animation update on a specific widget in the list.  

To do this, we're going to use `UI.forwardTo`.  Essentially, `UI.forwardTo` will take a list of things and an index, and apply an update to the thing at that index.  In order to do this, it needs a `getter` and a `setter` function to get and set the style of the widget.

```elm

forwardToWidget = UI.forwardTo 
                      Animate -- A way to create an Action.  Needs to be (Int -> UI.Action -> Your.Action)
                      .style -- widget style getter
                      (\w style -> { w | style = style }) -- widget style setter
```


Now that we have this function, we can animate a widget by using code like this:
```elm
      -- where i is the index of the widget we want to animate.
      let 
        (widgets, fx) = 
                    UI.animate
                        |> UI.duration (5*second)
                        |> UI.props 
                            [ UI.Opacity (UI.to 0)  
                            ] 
                        |> forwardToWidget i model.widgets

      in
        ( { model | widgets = widgets }
        , fx )

```


Because of this change, we also have to slightly change our `Animate` action to include the index of the widget that is being animated.

So, our Animate action gets an Int.

```elm

type Action = Animate Int UI.Action
```

And the update function forwards an animation update to a specific widget using our forwardToWidget.
```elm
    Animate i action ->
      let
        (widgets, fx) = 
            forwardToWidget i model.widgets action 
      in
        ( { model | widgets = widgets }
        , fx )

```


From here, we're able to animate each widget independently.


# Example 4: Stacking Transformations

[demo](https://mdgriffith.github.io/elm-html-animation/examples/StackingTransforms.html) / [view code](https://github.com/mdgriffith/elm-html-animation/blob/master/examples/StackingTransforms.elm)

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
initialWidgetStyle = UI.init 
                        [ UI.Rotate 0.0 UI.Deg
                        , UI.TranslateY 0.0 UI.Px
                        , UI.Rotate 0.0 UI.Deg
                        ]

```


Now let's animate these properties.  Let's say we want do the following animation:

  1. Make the first `UI.Rotate` move to 20deg.  
  2. Once that's finished, we want to translateY to -200px
  3. Now we want to rotate 1 revolution locally
  4. Now we want to rotate 1 revolution around the original center
  5. Then once that's finished, reset everything to 0.

```elm
  UI.animate 
      |> UI.duration (0.5*second)
      |> UI.props 
          [ UI.Rotate (UI.to 20) UI.Deg
          ] 
  |> UI.andThen
      |> UI.duration (0.7*second)
      |> UI.props 
          [ UI.TranslateY (UI.to -200) UI.Px
          ] 
  |> UI.andThen
      |> UI.duration (0.7*second)
      |> UI.props 
          [ UI.Rotate UI.stay UI.Deg  -- <-  Here's the only new function! 
                                      --  UI.stay allows us to specify 
                                      --  the 2nd Rotate we mentioned in our init
          , UI.Rotate (UI.to 360) UI.Deg
          ] 
    |> UI.andThen
      |> UI.duration (0.7*second)
      |> UI.props 
          [ UI.Rotate (UI.to 380) UI.Deg 
          ] 
  |> UI.andThen
      |> UI.delay (1*second)
      |> UI.props 
          [ UI.Rotate (UI.to 0.0) UI.Deg
          , UI.TranslateY (UI.to 0.0) UI.Px
          , UI.Rotate (UI.to 0.0) UI.Deg
          ] 

```

There's only one new function and that's `UI.stay`.

`UI.stay` just keeps a property at it's current value.  Normally this is just assumed for any property that isn't explicitly animated, but we need `UI.stay` in this case to access that second `UI.Rotate` property.


# Example 5: Staggering Animations

[demo](https://mdgriffith.github.io/elm-html-animation/examples/Stagger.html) / [view code](https://github.com/mdgriffith/elm-html-animation/blob/master/examples/Stagger.elm)

Staggering animations can add a very pleasing effect to an animation.

Staggering is running an animation on a list of things, and having that thing's index in that list affect some part of the animation.

A classic example is to delay each element of the list just slightly compared the the previous one.

How can we do this?  Well, essentially a stagger is just a function that takes the element's position and returns the animation it should have.  So that's what we do!


```elm

UI.stagger
      (\i -> 
         UI.animate
           |> UI.delay (i * 0.05 * second) -- i is the index of the widget in the list.
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


```


The cool part about this way of approaching it is that you can stagger anything you could normally animate: the delay, duration, props, etc. 


# Notes and Common Pitfalls


## Uninitialized Style Animations

If you attempt to animate a property that hasn't been provided in the initial style, _that property will not be animated._

This was done because it seems to be the better option than animating with an arbitrarily chosen default value and with arbitrarily chosen default units.

So, if a property isn't animating, check your UI.init!


## Doubly Specified Units

If you specify different units for a property in the UI.init vs the animation, _that property animation will also not be animated._

I know this is not the best solution, but there are a few reasons why it's the case.

 1. Html.Animation can't automatically convert between units.  A classic example of why not, would be attempting to convert px to %. This can't be done without querying the DOM.

 2. Again we want to avoid making up arbitrary default values.

I'm currently working on a solution by making units only specified in the UI.init, however this turns out to be fairly complicated (see __eliminating-twice-declared-units__ branch if you're curious and want a headache.)  

So, maybe this will be solved in the future?


## Slow?

If you're implementing a lot of animationatable things, you'll want to use [Html.Lazy](https://github.com/evancz/elm-html/blob/master/src/Html/Lazy.elm).  Basically any function that creates Html can be called with Html.lazy

```elm
-- So a function that has this signature
view : Model -> Html

-- would be called as
Html.Lazy.lazy2 view model

-- instead of just 
view model

-- Instant performance!

```






