import Time exposing (Time, second)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy
import Html.Animation as UI
import Html.Animation.Properties exposing (..)
import Html.App as Html
import AnimationFrame
--import Easing exposing (easeInBounce, easeInSine, easeOutSine)

type alias Model = { widgets : List Widget }


type alias Widget = 
          { label : String
          , style : UI.Animation
          , action : (Int -> Action)
          }

type Action = RotateWidget Int
            | RotateAllAxis Int
            | RotateCustomEasingDuration Int
            | ChangeColors Int
            | ChangeMultipleColors Int
            | FadeOutFadeIn Int
            | FadeOut Int
            | Loopty Int
            | Spring Int
            | Animate Time

                                    

mapToIndex : Int -> (a -> a) -> List a -> List a
mapToIndex j fn list =
          List.indexedMap
              (\i x ->
                if i == j then
                  fn x
                else
                  x
              ) list




update : Action -> Model -> ( Model, Cmd Action )
update action model =
  case action of
    RotateWidget i ->
      let 
        widgets = 
          UI.queue -- queue up this animation 
                   -- as opposed to interrupting
              |> UI.duration (1*second)
              |> UI.props 
                  [ Rotate (UI.add 1) Turn 
                  ] 
              |> (\act -> mapToIndex i 
                              (\widget -> 
                                  { widget 
                                      | style = UI.on widget.style act
                                  }
                              ) model.widgets)
      in
        ( { model | widgets = widgets }
        , Cmd.none )

    RotateAllAxis i ->
      let 
        widgets = 
          UI.queue -- queue up this animation 
                   -- as opposed to interrupting
              |> UI.duration (1*second)
              |> UI.props 
                  [ RotateX (UI.add 1) Turn
                  , RotateY (UI.add 1) Turn
                  , Rotate (UI.add 1) Turn
                  ] 
               |> (\act -> mapToIndex i 
                              (\widget -> 
                                  { widget 
                                      | style = UI.on widget.style act
                                  }
                              ) model.widgets)
      in
        ( { model | widgets = widgets }
        , Cmd.none )



    RotateCustomEasingDuration i ->
      let 
        widgets = 
          UI.queue -- queue up this animation 
                    -- as opposed to interrupting
            --|> UI.duration (2*second)
            --|> UI.easing easeInBounce
            |> UI.props 
                [ Rotate (UI.add 1) Turn 
                ] 
            |> (\act -> mapToIndex i 
                  (\widget -> 
                      { widget 
                          | style = UI.on widget.style act
                      }
                  ) model.widgets)
      in
        ( { model | widgets = widgets }
        , Cmd.none )


    Loopty i ->
      let 
        widgets = 
                UI.queue -- queue up this animation 
                         -- as opposed to interrupting
                    --|> UI.duration (0.5*second)
                    --|> UI.easing easeInSine
                    |> UI.props 
                        [ Rotate (UI.add -0.5) Turn
                        , TranslateY (UI.to 50) Px
                        , Rotate (UI.add 0.5) Turn
                        ] 
                  |> UI.andThen
                    --|> UI.duration (0.5*second)
                    --|> UI.easing easeOutSine
                    |> UI.props 
                        [ Rotate (UI.add -0.5) Turn
                        , TranslateY (UI.to 0) Px
                        , Rotate (UI.add 0.5) Turn
                        ] 
                   |> (\act -> mapToIndex i 
                              (\widget -> 
                                  { widget 
                                      | style = UI.on widget.style act
                                  }
                              ) model.widgets)
      in
        ( { model | widgets = widgets }
        , Cmd.none )


    Spring i ->
      let 
        widgets = 
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
             |> (\act -> mapToIndex i 
                        (\widget -> 
                            { widget 
                                | style = UI.on widget.style act
                            }
                        ) model.widgets)
      in
        ( { model | widgets = widgets }
        , Cmd.none )




    ChangeColors i ->
       let 
          widgets = 
            UI.animate -- animate is used to interrupt whatever current animation
                       -- is running and smoothely move to the new style
                |> UI.props 
                    [ BackgroundColor 
                          |> UI.toRGBA 100 100 100 1.0 
                    , BorderColor 
                          |> UI.toRGBA 100 100 100 1.0 
                    ] 
                 |> (\act -> mapToIndex i 
                              (\widget -> 
                                  { widget 
                                      | style = UI.on widget.style act
                                  }
                              ) model.widgets)
                      
        in
          ( { model | widgets = widgets }
          , Cmd.none )


    ChangeMultipleColors i ->
       let 
          widgets = 
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
             |> (\act -> mapToIndex i 
                            (\widget -> 
                                { widget 
                                    | style = UI.on widget.style act
                                }
                            ) model.widgets)

        in
          ( { model | widgets = widgets }
          , Cmd.none )

    FadeOutFadeIn i ->
       let 
          widgets =
            UI.animate
                |> UI.props 
                    [ Opacity (UI.to 0)  
                    ] 
            |> UI.andThen
                |> UI.props 
                    [ Opacity (UI.to 1)  
                    ] 
             |> (\act -> mapToIndex i 
                            (\widget -> 
                                { widget 
                                    | style = UI.on widget.style act
                                }
                            ) model.widgets)
        in
          ( { model | widgets = widgets }
          , Cmd.none )

    FadeOut i ->
       let 
         widgets =
          UI.animate
              |> UI.props 
                  [ Opacity (UI.to 0)
                  ] 
           |> UI.andThen
              |> UI.set 
                  [ Display None
                  ]
            |> (\act -> mapToIndex i 
                            (\widget -> 
                                { widget 
                                    | style = UI.on widget.style act
                                }
                            ) model.widgets)
        in
          ( { model | widgets = widgets }
          , Cmd.none )


    Animate time ->
      let 
        --_ = Debug.log "test" (toString time)
        newWidgets = 
           List.map 
              (\widget ->
                  { widget | style = UI.tick time widget.style }
              ) 
              model.widgets
      in
        ( { model 
              | widgets = newWidgets
          }
        , Cmd.none)


-- VIEW

view : Model -> Html Action
view model =
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
             (\i w -> Html.Lazy.lazy2 box i w) 
             model.widgets
                  


box : Int -> Widget -> Html Action
box i widget = 
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
          , onClick (widget.action i) ]
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



init : ( Model, Cmd Action )
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
    Cmd.none )


subscriptions : Model -> Sub Action
subscriptions model =
  AnimationFrame.times Animate

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }









