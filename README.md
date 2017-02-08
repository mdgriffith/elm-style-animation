# The Style Animation library for Elm!


## The Basics

To get started, there are a few things that need to happen.


__Set an initial style__ in your model.

```elm
import Animation exposing (px)

init : Model
init =
    { style = 
        Animation.style
            [ Animation.left (px 0.0)
            , Animation.opacity 1.0
            ]
    }
```

__Subscribe to Animation's subscription.__  This will animate using AnimationFrame when something is running, and stop giving updates when there is no animation. 
```elm
subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate [ model.style ]

```


__Set up an update `Msg`__ in your update function.
```elm
    Animate animMsg ->
        { model
            | style = Animation.update animMsg model.style
        }
```


__Render our animation__ at the necessary element in your view.  Not all animated properties are style properties(such as the svg.d property and polygon.points property), so `Animation.render` actually returns a list of `Html.Attributes`.  Fortunately, you can add your own style because  `Html.Attributes.style` stacks!
```elm
    div
        (List.concat
            [ Animation.render model.style
            , [ style
                    [ ( "position", "absolute" )
                    , ( "border-style", "dotted" )
                    ]
               ]
            ]
        )
        [ text "This is being Animated!" ]
```


__Start an animation__ in your update statement.

```elm
case msg of
    Show ->
        let 
            newStyle = 
                Animation.interrupt
                    [ Animation.to 
                        [ Animation.left (px 0.0)
                        , Animation.opacity 1.0
                        ]
                    ]
                    model.style
        in
            { model
                | style = newStyle
            }
```

Here's generally how we compose animations.

 * Choose `Animation.queue` or `Animation.interrupt`, both of which take a list of steps and your animation model.  This describes what the strategy should be if the thing you're trying to animate is already in the process of being animated.  You either want to interrupt what its doing and do this new animation.  Or you want to queue up this new animation to run after the current animation is finished.  90% of the time you want `Animation.interrupt`
 * Steps can be
    * `Animation.to` - Animate to a target style
    * `Animation.set` - Set a animation to a style immediately.
    * `Animation.wait (5 * second)` - wait for some amount of time
    * `Animation.repeat x [..list of steps to repeat]` - Repeat a list of steps x times.
    * `Animation.loop [..list of steps to repeat]` - Loop a list of steps forever/until interrupted.


# Examples

 * Simple FadeIn/FadeOut - [Code](https://github.com/mdgriffith/elm-style-animation/blob/master/examples/SimpleFadeIn.elm)

 * Menu Slide in - [Demo](https://mdgriffith.github.io/elm-style-animation/3.0.0/FlowerMenu/) - [Code](https://github.com/mdgriffith/elm-animation-flower-menu)

 * Rotating Gears - [Demo](https://mdgriffith.github.io/elm-style-animation/3.0.0/Gears.html) - [Code](https://github.com/mdgriffith/elm-style-animation/blob/master/examples/Gears.elm)

 * Logo (Svg polygon morphing) - [Demo](https://mdgriffith.github.io/elm-style-animation/3.0.0/Logo.html) - [Code](https://github.com/mdgriffith/elm-style-animation/blob/master/examples/Logo.elm)
 * Showcase - [Demo](https://mdgriffith.github.io/elm-style-animation/3.0.0/Showcase.html) - [Code](https://github.com/mdgriffith/elm-style-animation/blob/master/examples/Showcase.elm)

# Advanced!

## Note!

The compiler is going to refer to your animation model as `Animation.Model.Animation msg`.  `Animation.State` is just a synonym for that.

## Sending Messages

 * Send Messages Example - [Code](https://github.com/mdgriffith/elm-style-animation/blob/master/examples/SimpleSendMsg.elm)

First, import `Animation.Messenger`

Change your `Animation.State` to `Animation.Messenger.State MyMsgType`.

You can now use `Animation.Messenger.send MyCustomMessage` as a step in composing your animation.

You need to update this new animation state using `Animation.Messenger.update`, which will return `(newAnimState, messagesSentinCmdForm)`.  So you need to change your animation update section to something like the following.




```elm
case msg of
    Animate animMsg ->
        let
            (newStyle, cmds) =
                Animation.Messenger.update
                    animMsg
                    model.style
        in
            ( { model
                 | style = newStyle
              },
              cmds
            )
```

_Note!_ Make sure you're sending the cmds in the above code.  If you're note, then the animation will run, but the messages won't be sent.

Also, if you're running this in a child component, make sure you're `Cmd.map`ing the child's commands back to the child or else the messages will be lost!


## Animating Properties that aren't directly supported.

You can construct custom properties if you don't find them in the library using `Animation.custom`.  These will be rendered in the style property.

```elm
Animation.to
    [ Animation.custom "my-custom-prop" 5 "px"
    ]

```

There is also `customColor` for color based properties.



## Setting Custom Interpolation

Behind the curtain elm-style-animation mostly uses springs to animate values from A to B.  However you can specify custom values for a spring, or a duration and easing if you want. There are two basic ways to do this.


### Set them with your initial style.

Use `Animation.styleWith` or `Animation.styleWithEach` to set your initial style instead of `Animation.style`.  

```elm
Animation.styleWith
    (Animation.spring
        { stiffness = 400
        , damping = 23 }
    )
    [ Animation.opacity 0
    , Animation.left (px 20)
    ]
```

This will set the spring used for these properties.  Alternatively `Animation.styleWithEach` is a way to set a custom interpolation for each individual property.


### Set a temporary spring/duration + easing

You can also use `Animation.toWith` and `Animation.toWithEach`.  These can be substituted for `Animation.to` and allow you to specify a spring or duration+easing that lasts for exactly one step.  After that step, whatever default spring or duration/easing there is (either the auto default or via being specified in `Animation.styleWith`) is then used.

```elm
Animation.interrupt
    [ Animation.toWith
        (Animation.easing
            { duration = 2*second
            , ease = (\x -> x^2)
            }
        ) 
        [ Animation.left (px 0.0)
        , Animation.opacity 1.0
        ]
    ]
    model.style
```
