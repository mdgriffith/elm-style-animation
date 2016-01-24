
# elm-html-animation

A library to simplify creating html animations in elm.  

Try it out!

First have [Elm installed](http://elm-lang.org/install), then

```bash
$ git clone https://github.com/mdgriffith/elm-html-animation.git
$ cd elm-html-animation/examples
$ elm-reactor
```

My focus was to create something I could use as a UI designer to prototype animations quickly, accurately, and without sneaky errors.


ElmHtmlAnimations supports __Declarative animations__, which tend to be very intuitive. State what the style should be, and it will animate to that style. 

 * __Animations can be smoothly interrupted.  Or chained together.__  

 * __Relative animation__ such as using `+=` and `-=` to animate a property based on its previous value is also available.  Though, be aware that mixing this and animation interruption can lead to unanticipated results.

 * __CSS properties and units are type checked.__ 

 * __Use custom easing__ such as all the lovely functions in [this library](http://package.elm-lang.org/packages/Dandandan/Easing/2.0.1/Easing#easing-functions).

 * __Manage multiple animations__ by using the `forwardTo` function.  See the _Showcase_ example to see how it works.

 * __Infrastructure for more complicated animations__ is provided.  Want to do something like animate a position along a path?  You have the tools to do that.  (If you're curious, you'd start by writing a custom `to` function.)


## The Basics

I recommend checking out [the Elm Architecture](https://github.com/evancz/elm-architecture-tutorial/) if you haven't already.  These examples will be much easier if you're already familiar with the standard model, update, and view pattern.  As well you should understand the basics of using the `Effects` library.

So, with all that in mind, here's a basic overview of what you'll need to do to use this library.

To add animations to a module, you'll need to do the following:

  1. Store the styling data in your `model`, and define an initial style.
  __Note__ all properties that are going to be animated need to be accounted for in the initial style.

```elm
import HtmlAnimation as UI

type alias Model = { style : UI.StyleAnimation }

init : Model
init = { style = 
            UI.initStyle 
                [ UI.Left -350.0 UI.Px
                , UI.Opacity 0.0 
                ]
        }
```

  2. Add a new action to your Action type to allow updates to be sent to an animation.

```elm
type Action = Animate UI.StyleAction

-- and in our update function, we need the following code to allow for updates to be passed to an animation

update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of

    Animate action ->
      let
        (anim, fx) = UI.updateStyle action model.style
      in
        ( { model | style = anim }
        , Effects.map Animate fx )
```

  3. In your `view`, render the animation as a css style.

```elm

view : Address Action -> Model -> Html
view address model =
          div [ style (UI.render model.style) ] []

```


  4. Start creating animations!  Now that we're set up, we can begin animating. 


# Example 1: Showing a Menu on Hover
[demo](https://mdgriffith.github.io/elm-html-animation/examples/SideMenu.html) / [view code](https://github.com/mdgriffith/elm-html-animation/blob/master/examples/SideMenu.elm)

Our first example is a menu that is shown when the mouse enters a certain area, and hides when the mouse leaves.

So, our first step is to add two values to our Action type, `Show` and `Hide`, and in the update function, we start an animation when those actions occur.  Let's take a look at how we construct an animation.


```elm
-- ... we add this to the case statement in our update function.
    Show ->
      let 
        (anim, fx) = 
              UI.animateOn model.style
              -- <| UI.duration (0.5*second)
              -- <| UI.easing (\x -> x)
                 <| UI.props 
                     [ UI.Left (UI.to 0) UI.Px
                     , UI.Opacity (UI.to 1)
                     ] 
                <| [] -- every animation has 
                      -- to be 'started' with an empty list

      in
        ( { model | style = anim }
        , Effects.map Animate fx )
```

Notice we are programming declaratively by defining what style property should be by using `UI.to`.  

We have the option of defining a duration and an easing function. In the above code, these are commented out, which means the defaults are used.  The default duration is _400ms_, while the default easing is _sinusoidal in-out_.  Make sure to check out this [library](http://package.elm-lang.org/packages/Dandandan/Easing/2.0.1/Easing#easing-functions) if you're looking for easing functions.

Finally, we also have to `Effects.map` the resulting effect(fx) in order to route it to the Animate action we created earlier.

Now that we have this animation, it has a few properties that may not be immediately apparent.  If a `Hide` action is called halfway through execution of the `Show` animation, the animation will be smoothly interrupted. 




# Example 2: Chaining Animations
[demo](https://mdgriffith.github.io/elm-html-animation/examples/Chaining.html) / [view code](https://github.com/mdgriffith/elm-html-animation/blob/master/examples/Chaining.elm)

We also have option of chaining animations together.  So, let's make a square that cycles through a few background colors when you click it.

```elm
-- in our update function:
    ChangeColor ->
      let 
        (anim, fx) = 
            UI.animateOn model.style
                    <| UI.props 
                        [ UI.BackgroundColorA 
                              UI.RGBA (UI.to 100) (UI.to 100) (UI.to 100) (UI.to 1.0)  
                        ] 
                <| UI.andThen -- create a new keyframe
                    <| UI.duration (1*second)
                    <| UI.props 
                        [ UI.BackgroundColorA 
                              UI.RGBA (UI.to 178) (UI.to 201) (UI.to 14) (UI.to 1.0) 
                        ] 
                <| UI.andThen 
                    <| UI.props 
                        [ UI.BackgroundColorA 
                              UI.RGBA (UI.to 58) (UI.to 40) (UI.to 69) (UI.to 1.0) 
                        ] 
                <| [] 

      in
        ( { model | style = anim }
        , Effects.map Animate fx )

```


In this case we can use `UI.andThen` to create a new key frame.  This new keyframe will have it's own independent duration, easing, and properties.  Again, it can be interrupted at any point.


# Example 3: Managing Multiple Animations
[demo](https://mdgriffith.github.io/elm-html-animation/examples/Showcase.html) / [view code](https://github.com/mdgriffith/elm-html-animation/blob/master/examples/Showcase.elm)




