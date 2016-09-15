

# Elm Style Animation 3.0.0


`Style.renderAttr` renamed to `Style.attrs`.  This shortening was due to how common this function may be in view code. 


# Elm Style Animation 2.0.0

Update syntax:

New (2.0.0)

```
Style.update
      [ Rotate identity Deg  -- Skip the first update in the transform stack
      , Rotate (\_ -> 360) Deg
      ]
```


Old (1.0.0)

```
Style.update
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

```




# Elm Style Animation 1.0.0 (and transitioning from elm 0.16)


Here's an overview of the syntax changes.

## Import names
elm-html-animation 3.0.4 (old) - on elm 0.16
```elm

import Html.Animation as UI
import Html.Animation.Properties exposing (..)

```


elm-html-animation 3.0.4 (new) - on elm 0.17
```elm
-- imports are different
import AnimationFrame
import Style
import Style.Properties


type Msg 
    = Show
    | Animate Time

-- Also, you'll have to subscribe to AnimateFrame.times
-- Animate should be in your action/msg type, and it should take a 'time'
subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.times Animate



main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

```


## Specifying Animations
elm-html-animation 3.0.4 (old) - on elm 0.16
```elm

-- specify helper functions
onMenu =
  UI.forwardTo 
      Animate
      .style
      (\w style -> { w | style = style }) -- style setter 


--... in your update function
    UI.animate
       |> UI.props
           [ Left (UI.to 0) Px
           , Opacity (UI.to 1)
           ]
       |> onMenu model -- the on

  

```


elm-html-animation 3.0.4 (new) - on elm 0.17
```elm
    
    -- helper functions are no longer required
    Style.animate
       -- styles are specified slightly differently.
       |> Style.to
           [ Left 0 Px
           , Opacity 1
           ]
       |> Style.on model.style
   
```


## Ticking animations forward
elm-html-animation 3.0.4 (old) - on elm 0.16
```elm

-- specify helper functions
onMenu =
  UI.forwardTo 
      Animate
      .style
      (\w style -> { w | style = style }) -- style setter 


--... use helper function
    Animate action ->
      onMenu model action
  

```


elm-html-animation 3.0.4 (new) - on elm 0.17
```elm
    -- again, no helper functions.
    -- under the message that you subscribed to with animationFrame, step your animations.
    Animate time ->
        ( { model
            | style = Style.tick time model.style
          }
        , Cmd.none
        )
```

