
# elm-html-animation

A library to simplify creating html animations in elm.  

Try it out!
[First have Elm installed](http://elm-lang.org/install), then

```
$ git clone https://github.com/mdgriffith/elm-html-animation.git
$ cd elm-html-animation/examples
$ elm-reactor
```

My focus was to create something I can use to prototype quickly and accurately as a UI designer.


 * CSS Units are type checked
 * Declarative animations.  State what the style should be, and it will animate to that style.
 * Animations can be smoothely interrupted.
 * Custom easing functions can be used (such as [these](http://package.elm-lang.org/packages/Dandandan/Easing/2.0.1/Easing#easing-functions))
 * += and -= can be used to animate a property based on its previous value
 * Infrastructure for more complicated animations, such as moving along a path, is provided.


## A rough sketch on how to get started

Check out the examples for complete code, but to get you started, here's how you would use this library.


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




