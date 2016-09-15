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
    Animation.subscription [model.style] Animate

```


__Set up an update `Msg`__ in your update function.
```elm
    Animate animMsg ->
        { model
            | animation = Animation.update animMsg model.style
        }
                
```


__Render our animation__ at the necessary element in your view.  Not all animated properties are style properties(such as the svg.d property and polygon.points property), so `Animation.render` actaully returns a list of `Html.Attributes`.  Fortunately, you can add your own style because  `Html.Attributes.style` stacks!
```elm
    div
        (Animation.render model.style
            ++ [ style
                    [ ( "position", "absolute" )
                    , ( "border-style", "dotted" )
                    ]
               ]
        )
        [ text "This is being Animated!" ]
```




__Start an animation__ in your update statement.

```elm
case msgs of
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

 * Choose `Animation.queue` or `Animation.interrupt`, both of which take a list of steps and your animation model.
 * Steps can be
    * `Animation.to` - Animate to a target style
    * `Animation.set` - Set a animation to a style immediately.
    * `Animation.wait (5 * second)` - wait for some amount of time
    * `Animation.repeat x [..list of steps to repeat]` - Repeat a list of steps x times.
    * `Animation.loop [..list of steps to repeat]` - Loop a list of steps forever/until interrupted.


# Examples
 * Gears - [Demo](https://mdgriffith.github.io/elm-style-animation/3.0.0/Gears.html) - [Code](https://github.com/mdgriffith/ui-animation/blob/master/examples/Gears.elm)
 * Menu - [Demo](https://mdgriffith.github.io/elm-style-animation/3.0.0/FlowerMenu/) - [Code](https://github.com/mdgriffith/elm-html-animation-flower-menu/tree/split-out-animation)
 * Logo - [Demo](https://mdgriffith.github.io/elm-style-animation/3.0.0/Logo.html) - [Code](https://github.com/mdgriffith/ui-animation/blob/master/examples/Logo.elm)
 * Showcase - [Demo](https://mdgriffith.github.io/elm-style-animation/3.0.0/Showcase.html) - [Code](https://github.com/mdgriffith/ui-animation/blob/master/examples/Showcase.elm)


