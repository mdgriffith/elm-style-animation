

import StartApp exposing (start)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects exposing (Effects, Never)
import Signal exposing (Address)
import Task

import Time exposing (second)

import Html.Animation as UI



type alias Model = 
            { widgets : List Widget
            }

type alias Widget = 
            { style : UI.Animation 
            } 
-- UPDATE

type Action = Show 
            | Animate Target UI.Action

type Target = OnWidget Int



forwardToWidget = UI.forwardTo 
                      (\i action -> Animate (OnWidget i) action)
                      .style -- widget style getter
                      (\w style -> { w | style = style }) -- widget style setter
                                    
forwardToAllWidgets = UI.forwardToAll 
                          (\i action -> Animate (OnWidget i) action)
                          .style -- widget style getter
                          (\w style -> { w | style = style }) -- widget style setter


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Show ->
      let 
        (widgets, fx) = 
              UI.stagger
                (\i -> 
                   UI.animate
                     |> UI.delay (i * 0.05 * second)
                     |> UI.duration (0.3 * second)
                     |> UI.props 
                         [ UI.Left (UI.to 200) UI.Px
                         ] 
                  |> UI.andThen
                     |> UI.delay (2.0 * second)
                     |> UI.duration (0.3 * second)
                     |> UI.props 
                         [ UI.Left (UI.to -50) UI.Px
                         ] 
                )
                |> forwardToAllWidgets model.widgets

      in
        ( { model | widgets = widgets }
        , fx )

    Animate target action ->
      case target of
        OnWidget i ->
          let
            (widgets, fx) = forwardToWidget i model.widgets action
          in
            ( { model | widgets = widgets }
            , fx )



view : Address Action -> Model -> Html
view address model =
            let
              triggerStyle = [ ("position", "relative")
                             , ("left", "0px")
                             , ("top", "0px")
                             , ("width", "300px")
                             , ("margin-top", "250px")
                             , ("margin-left", "auto")
                             , ("margin-right", "auto")
                             , ("padding", "25px")
                             , ("text-align", "center")
                             , ("border-radius", "5px")
                             , ("background-color", "#AAA")
                             , ("cursor", "pointer")
                            ]
            in
              div [ onClick address Show
                  , style triggerStyle  
                  ]

                  ([ h1 [ style [("padding","25px")]] 
                        [ text "Click me!"]
                   , p [] [ text "This example shows staggered animations"]
                  
                  ] ++ List.map (viewWidget address) model.widgets)

viewWidget : Address Action -> Widget -> Html
viewWidget address model =
                let
                  menuStyle =  [ ("border-radius", "20px")
                               , ("width", "40px")
                                , ("height", "40px")
                                , ("position", "fixed")
                                , ("background-color", "#4e9a06")
                                , ("top", "0px")
                                , ("z-index", "0")
                                , ("display", "inline-block")
                                , ("margin", "10px")
                                , ("text-align", "center")
                                , ("line-height", "40px")
                                ]
                in
                  div [ style (menuStyle ++ (UI.render model.style)) ]
                      [  ]


init : ( Model, Effects Action )
init = ( { widgets = List.map (\i -> initWidget i) [0..10]  }
       , Effects.none )

initWidget i = 
        { style = UI.init 
                      [ UI.Left -50.0 UI.Px
                      , UI.Top (i * 45.0) UI.Px
                      ]
         }


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



