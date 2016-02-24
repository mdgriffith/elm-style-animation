

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
import Html.Animation.Properties exposing (..)

import Easing exposing (easeInBounce, easeInSine, easeOutSine)


-- MODEL

type alias Model = { widgets : List Widget }


type alias Widget = 
          { label : String
          , style : UI.Animation
          , action : (Int -> Action)
          }


-- UPDATE

type Action = RotateWidget Int
            | RotateAllAxis Int
            | RotateCustomEasingDuration Int
            | ChangeColors Int
            | ChangeMultipleColors Int
            | FadeOutFadeIn Int
            | FadeOut Int
            | Loopty Int
            | Spring Int
            | Animate Int UI.Action

{-| Prepare a helper function manage effects and assign styles -}
onWidget = 
    UI.forwardToIndex
        Animate
        .style -- widget style getter
        (\w style -> { w | style = style }) -- widget style setter
                                    


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    RotateWidget i ->
      let 
        (widgets, fx) = 
              UI.queue -- queue up this animation 
                       -- as opposed to interrupting
                  |> UI.duration (1*second)
                  |> UI.props 
                      [ Rotate (UI.add 1) Turn 
                      ] 
                  |> onWidget i model.widgets 
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
                      [ RotateX (UI.add 1) Turn
                      , RotateY (UI.add 1) Turn
                      , Rotate (UI.add 1) Turn
                      ] 
                  |> onWidget i model.widgets 
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
                      [ Rotate (UI.add 1) Turn 
                      ] 
                  |> onWidget i model.widgets 
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
                        [ Rotate (UI.add -0.5) Turn
                        , TranslateY (UI.to 50) Px
                        , Rotate (UI.add 0.5) Turn
                        ] 
                  |> UI.andThen
                    |> UI.duration (0.5*second)
                    |> UI.easing easeOutSine
                    |> UI.props 
                        [ Rotate (UI.add -0.5) Turn
                        , TranslateY (UI.to 0) Px
                        , Rotate (UI.add 0.5) Turn
                        ] 

                  |> onWidget i model.widgets 
      in
        ( { model | widgets = widgets }
        , fx )


    Spring i ->
      let 
        (widgets, fx) = 
                UI.queue 
                    |> UI.spring UI.noWobble
                    |> UI.props 
                        [ Scale (UI.to 1.5)
                        ] 
                  |> UI.andThen
                    |> UI.spring UI.wobbly
                    |> UI.props 
                        [ Scale (UI.to 1.0)
                        ] 
                  |> onWidget i model.widgets 
      in
        ( { model | widgets = widgets }
        , fx )




    ChangeColors i ->
       let 
          (widgets, fx) = 
                  UI.animate -- animate is used to interrupt whatever current animation
                             -- is running and smoothely move to the new style
                      |> UI.props 
                          [ BackgroundColor 
                                |> UI.toRGBA 100 100 100 1.0 
                          , BorderColor 
                                |> UI.toRGBA 100 100 100 1.0 
                          ] 
                      |> onWidget i model.widgets
                      
        in
          ( { model | widgets = widgets }
          , fx )


    ChangeMultipleColors i ->
       let 
          (widgets, fx) = 
                      UI.animate -- animate is used to interrupt whatever current animation
                                 -- is running and smoothely move to the new style
                          |> UI.props 
                              [ BackgroundColor
                                    |> UI.toRGBA 100 100 100 1.0 
                              , BorderColor
                                    |> UI.toRGBA 100 100 100 1.0  
                              ] 
                      |> UI.andThen 
                          |> UI.props 
                              [ BackgroundColor
                                    |> UI.toRGBA 178 201 14 1.0 
                              , BorderColor
                                    |> UI.toRGBA 178 201 14 1.0 
                              ] 
                      |> onWidget i model.widgets

        in
          ( { model | widgets = widgets }
          , fx)

    FadeOutFadeIn i ->
       let 
          (widgets, fx) =
                    UI.animate
                        |> UI.props 
                            [ Opacity (UI.to 0)  
                            ] 
                    |> UI.andThen
                        |> UI.props 
                            [ Opacity (UI.to 1)  
                            ] 
                    |> onWidget i model.widgets

        in
          ( { model | widgets = widgets }
          , fx )

    FadeOut i ->
       let 
          (widgets, fx) =
                    UI.animate
                        |> UI.props 
                            [ Opacity (UI.to 0)
                            ] 
                     |> UI.andThen
                        |> UI.set 
                            [ Display None
                            ]
                     |> onWidget i model.widgets

        in
          ( { model | widgets = widgets }
          , fx )


    Animate i action ->
      let
        (widgets, fx) = onWidget i model.widgets action
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

                  <| List.indexedMap 
                       (\i w -> Html.Lazy.lazy2 (box address) i w) 
                       model.widgets
                  


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
                        [ Display InlineBlock
                        , Rotate 0.0 Turn
                        , RotateX 0.0 Turn
                        , RotateY 0.0 Turn
                        , TranslateY 0.0 Px
                        , Rotate 0.0 Turn
                        , Opacity 1
                        , BackgroundColor |> UI.rgba 58 40 69 1.0
                        , Color |> UI.rgba 255 255 255 1.0
                        , Scale 1.0
                        , BorderColor |> UI.rgb 136 96 161
                        , BorderWidth 4 Px 
                        , BorderRadius 8 Px
                        ]



init : ( Model, Effects Action )
init = (

      { widgets =  
          [  
              { label = "Rotate"
              , style = initialWidgetStyle
              , action = RotateWidget
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
              { label = "Fade Out Fade In"
              , style = initialWidgetStyle
              , action = FadeOutFadeIn
              }
          , 
              { label = "Fade Out and display:none"
              , style = initialWidgetStyle
              , action = FadeOut
              }
          , 
              { label = "Loop About"
              , style = initialWidgetStyle
              , action = Loopty
              }
          , 
            { label = "Use a Spring"
            , style = initialWidgetStyle
            , action = Spring
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









