

import StartApp exposing (start)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects exposing (Effects, Never)
import Task

import Time exposing (Time, second)
import Signal exposing (Address)

import HtmlAnimation as UI


-- MODEL

type alias Model = { widgets : List Widget }


type alias Widget = 
          { label : String
          , style : UI.Model
          , action : (Int -> Action)
          }


-- UPDATE

type Action = Rotate Int
            | ChangeColors Int
            | Animate Int UI.Action


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Rotate i ->
      let 
        (widgets, fx) = 
            forwardToWidget i model.widgets 
                  <| UI.animate
                  <| UI.duration (2*second)
                  <| UI.props 
                      [ UI.RotateX UI.Turn (UI.add 1)
                      , UI.RotateY UI.Turn (UI.add 1)
                      , UI.Rotate UI.Turn (UI.add 1)
                      ]
      in
        ( { model | widgets = widgets }
        , Effects.map (Animate i) fx )


    ChangeColors i ->
       let 
          (widgets, fx) = 
              forwardToWidget i model.widgets
                    <| UI.animate
                    <| UI.duration (2*second)
                    <| UI.props 
                        [ UI.BackgroundColorA 
                              UI.RGBA (UI.to 255) (UI.to 0) (UI.to 0) (UI.to 1.0) 
                        , UI.ColorA 
                              UI.RGBA (UI.to 0) (UI.to 0) (UI.to 0) (UI.to 1.0) 
                        ] 
                        
        in
          ( { model | widgets = widgets }
          , Effects.map (Animate i) fx )


    Animate i action ->
      let
        (widgets, fx) = 
            forwardToWidget i model.widgets 
                  <| UI.update action 
      in
        ( { model | widgets = widgets }
        , Effects.map (Animate i) fx )


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

                  (List.indexedMap (\i w -> box address i w) model.widgets)


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
                                , ("vertical-align", "middle")
                              ]
                in
                  div [ style (boxStyle ++ UI.render widget.style) 
                      , onClick address (widget.action i) ]
                      [ text widget.label ]




initialWidgetStyle = UI.initStyle 
                        [ UI.Rotate UI.Turn 0.0
                        , UI.RotateX UI.Turn 0.0
                        , UI.RotateY UI.Turn 0.0
                        , UI.Rotate UI.Turn 0.0
                        , UI.Opacity 1
                        , UI.BackgroundColorA UI.RGBA 58 40 69 1.0
                        , UI.ColorA UI.RGBA 255 255 255 1.0
                        , UI.Scale 1.0
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
              { label = "Change Colors"
              , style = initialWidgetStyle
              , action = ChangeColors
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




forwardToWidget : Int -> List Widget -> (UI.Model -> ( UI.Model, Effects UI.Action )) -> (List Widget, Effects UI.Action)
forwardToWidget i widgets anim = UI.forwardTo 
                                    i widgets -- index and list of widgets
                                    .style -- widget style getter
                                    (\w style -> { w | style = style }) -- widget style setter
                                    anim -- animation



