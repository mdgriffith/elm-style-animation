
# elm-html-animation

A library to simplify creating html animations in elm.  

Try it out!

First have [Elm installed](http://elm-lang.org/install), then

```
$ git clone https://github.com/mdgriffith/elm-html-animation.git
$ cd elm-html-animation/examples
$ elm-reactor
```

My focus was to create something I could use as a UI designer to prototype animations quickly, accurately, and without sneaky errors.


ElmHtmlAnimations supports __Declarative animations__, which tend to be very intuitive. State what the style should be, and it will animate to that style. 

__Animations can be smoothely interrupted.  Or chained together.__   Its up to you.  

__Relative animation__ such as using `+=` and `-=` to animate a property based on its previous value is also available.  Though, be aware mixing this and animation interruption can lead to unanticipated results.


__CSS properties and units are type checked.__  No more elusive property typos, it's all caught at compile-time!  Didn't know that the skew transform needs to have angle units?  No problemo, the compiler will let you know.  Didn't know that rgb colors _must_ be integers and not floats?  No worries, you don't need to know that.

__Use custom easing__ such as all the lovely functions in [this library](http://package.elm-lang.org/packages/Dandandan/Easing/2.0.1/Easing#easing-functions).

__Manage multiple animations__ by using the `forwardTo` function.  See the _Showcase_ example to see how it works.

__Infrastructure for more complicated animations__ is provided.  Want to do something like animate a position along a path?  You have the tools to do that.  (If you're curious, you'd start by writing a custom `to` function.)


## A rough sketch on how to get started

__Note__ Check out the examples for real reference code, this is only a rough overview.

Add a field to the model of the widget you want to animate.
In your initial model, you'll also have to provide an initial style.  


```elm
import HtmlAnimation as UI

type alias Model = { style : UI.Model }


init : Model
init = { style = 
            UI.initStyle 
                [ UI.Left UI.Px -350.0
                , UI.Opacity 0.0 
                ]
        }
```

__Note__ Your initial style should specify all the properties you intend to animate. 

In your update function, you will have to start the animation.

The animation itself is just a statement a target style
```elm
(anim, fx) = 
      animateOn model.style
         <| UI.duration (0.4*second)
         <| UI.props 
             [ UI.Left UI.Px (UI.to 0) 
             , UI.Opacity (UI.to 1)
             ]
         <| []
```


And finally, in your view function, you will have to render the style on the element and add any other styling you may already be providing.

```elm


view : Address Action -> Model -> Html
view address model =
        let
          menuStyle = [ ("position", "absolute")
                        , ("top", "0px")
                      ]
        in
          div [ style (menuStyle ++ (UI.render model.style)) ] []


```










