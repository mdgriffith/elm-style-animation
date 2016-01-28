

import StartApp exposing (start)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy
import Effects exposing (Effects, Never)
import Task

import Time exposing (Time, second)
import Signal exposing (Address)

import Html.Animation as UI
import Easing exposing (easeInBounce, easeInSine, easeOutSine)


-- MODEL

type alias Model = { widgets : List Widget }


type alias Widget = 
          { label : String
          , style : UI.Animation
          , action : (Int -> Action)
          }


-- UPDATE

type Action = Rotate Int
            | RotateAllAxis Int
            | RotateCustomEasingDuration Int
            | ChangeColors Int
            | ChangeMultipleColors Int
            | FadeInFadeOut Int
            | Loopty Int
            | Animate Int UI.Action


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Rotate i ->
      let 
        (widgets, fx) = 
              UI.queue -- queue up this animation 
                       -- as opposed to interrupting
                  |> UI.duration (1*second)
                  |> UI.props 
                      [ UI.Rotate (UI.add 1) UI.Turn 
                      ] 
                  |> forwardToWidget i model.widgets 
      in
        ( { model | widgets = widgets }
        , fx )

    RotateAllAxis i ->
      let 
        (widgets, fx) = 
              UI.queue -- queue up this animation 
                       -- as opposed to interrupting
                  |> UI.duration (1*second)
                  |> UI.props 
                      [ UI.RotateX (UI.add 1) UI.Turn
                      , UI.RotateY (UI.add 1) UI.Turn
                      , UI.Rotate (UI.add 1) UI.Turn
                      ] 
                  |> forwardToWidget i model.widgets 
      in
        ( { model | widgets = widgets }
        , fx )



    RotateCustomEasingDuration i ->
      let 
        (widgets, fx) = 
                UI.queue -- queue up this animation 
                          -- as opposed to interrupting
                  |> UI.duration (2*second)
                  |> UI.easing easeInBounce
                  |> UI.props 
                      [ UI.Rotate (UI.add 1) UI.Turn 
                      ] 
                  |> forwardToWidget i model.widgets 
      in
        ( { model | widgets = widgets }
        , fx )


    Loopty i ->
      let 
        (widgets, fx) = 
                UI.queue -- queue up this animation 
                         -- as opposed to interrupting
                    |> UI.duration (0.5*second)
                    |> UI.easing easeInSine
                    |> UI.props 
                        [ UI.Rotate (UI.add -0.5) UI.Turn
                        , UI.TranslateY (UI.to 50) UI.Px
                        , UI.Rotate (UI.add 0.5) UI.Turn
                        ] 
                  |> UI.andThen
                    |> UI.duration (0.5*second)
                    |> UI.easing easeOutSine
                    |> UI.props 
                        [ UI.Rotate (UI.add -0.5) UI.Turn
                        , UI.TranslateY (UI.to 0) UI.Px
                        , UI.Rotate (UI.add 0.5) UI.Turn
                        ] 

                  |> forwardToWidget i model.widgets 
      in
        ( { model | widgets = widgets }
        , fx )



    ChangeColors i ->
       let 
          (widgets, fx) = 
                  UI.animate -- animate is used to interrupt whatever current animation
                             -- is running and smoothely move to the new style
                      |> UI.props 
                          [ UI.BackgroundColor 
                                |> UI.toRGBA 100 100 100 1.0 
                          , UI.BorderColor 
                                |> UI.toRGBA 100 100 100 1.0 
                          ] 
                      |> forwardToWidget i model.widgets
                      
        in
          ( { model | widgets = widgets }
          , fx )


    ChangeMultipleColors i ->
       let 
          (widgets, fx) = 
                      UI.animate -- animate is used to interrupt whatever current animation
                                 -- is running and smoothely move to the new style
                          |> UI.props 
                              [ UI.BackgroundColor
                                    |> UI.toRGBA 100 100 100 1.0 
                              , UI.BorderColor
                                    |> UI.toRGBA 100 100 100 1.0  
                              ] 
                      |> UI.andThen 
                          |> UI.props 
                              [ UI.BackgroundColor
                                    |> UI.toRGBA 178 201 14 1.0 
                              , UI.BorderColor
                                    |> UI.toRGBA 178 201 14 1.0 
                              ] 
                      |> forwardToWidget i model.widgets

        in
          ( { model | widgets = widgets }
          , fx)

    FadeInFadeOut i ->
       let 
          (widgets, fx) =
                    UI.animate
                        |> UI.props 
                            [ UI.Opacity (UI.to 0)  
                            ] 
                    |> UI.andThen
                        |> UI.props 
                            [ UI.Opacity (UI.to 1)  
                            ] 
                    |> forwardToWidget i model.widgets

        in
          ( { model | widgets = widgets }
          , fx )


    Animate i action ->
      let
        (widgets, fx) = forwardToWidget i model.widgets action
      in
        ( { model | widgets = widgets }
        , fx )


-- VIEW

view : Address Action -> Model -> Html
view address model =
            let
              triggerStyle = [ ("position", "relative")
                             , ("left", "0px")
                             , ("top", "0px")
                             , ("width", "100%")
                             , ("height", "100%")

                            ]
            in
              div [ style triggerStyle ]

                  (List.indexedMap (\i w -> Html.Lazy.lazy (box address i) w) model.widgets)


box : Address Action -> Int -> Widget -> Html
box address i widget = 
               let
                  boxStyle = [ ("position", "relative")
                              , ("display", "inline-block")
                                , ("margin", "50px 50px")
                                , ("padding", "25px")
                                , ("text-align", "center")
                                , ("width", "100px")
                                , ("height", "100px")
                                , ("color", "white")
                                , ("cursor", "pointer")
                                , ("border-style", "solid")
                                , ("vertical-align", "middle")
                              ]
                in
                  div [ style (boxStyle ++ UI.render widget.style) 
                      , onClick address (widget.action i) ]
                      [ text widget.label ]




initialWidgetStyle = UI.init 
                        [ UI.Rotate 0.0 UI.Turn
                        , UI.RotateX 0.0 UI.Turn
                        , UI.RotateY 0.0 UI.Turn
                        , UI.TranslateY 0.0 UI.Px
                        , UI.Rotate 0.0 UI.Turn
                        , UI.Opacity 1
                        , UI.BackgroundColor |> UI.rgba 58 40 69 1.0
                        , UI.Color |> UI.rgba 255 255 255 1.0
                        , UI.Scale 1.0
                        , UI.BorderColor |> UI.rgb 136 96 161
                        , UI.BorderWidth 4 UI.Px 
                        , UI.BorderRadius 8 UI.Px
                        ]



init : ( Model, Effects Action )
init = (

      { widgets =  
          [  
              { label = "Rotate"
              , style = initialWidgetStyle
              , action = Rotate
              }
          , 
              { label = "Rotate in All Kinds of Ways"
              , style = initialWidgetStyle
              , action = RotateAllAxis
              }
          ,   
              { label = "Rotate with custom easing and duration"
              , style = initialWidgetStyle
              , action = RotateCustomEasingDuration
              }
          , 
              { label = "Change Colors"
              , style = initialWidgetStyle
              , action = ChangeColors
              }
          , 
              { label = "Change Through Multiple Colors"
              , style = initialWidgetStyle
              , action = ChangeMultipleColors
              }
          , 
              { label = "FadeIn FadeOut"
              , style = initialWidgetStyle
              , action = FadeInFadeOut
              }
          , 
              { label = "Loop About"
              , style = initialWidgetStyle
              , action = Loopty
              }
          ]
      }, 
    Effects.none )


app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }

main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks




forwardToWidget = UI.forwardTo 
                      Animate
                      .style -- widget style getter
                      (\w style -> { w | style = style }) -- widget style setter
                                    




