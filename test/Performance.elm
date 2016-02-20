

import StartApp exposing (start)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects exposing (Effects, Never)
import Signal exposing (Address)
import Task

import Time exposing (second)

import Html.Animation as UI
import Html.Animation.Properties exposing (..)
import Debug

import Html.Lazy


type alias Model = 
            { widgets : List Widget
            }

type alias Widget = 
            { style : UI.Animation 
            } 
-- UPDATE

type Action = Begin
            | Animate Int UI.Action


forwardToWidget = UI.forwardTo 
                      Animate
                      .style -- widget style getter
                      (\w style -> { w | style = style }) -- widget style setter
                                    
forwardToAllWidgets = UI.forwardToAll 
                          Animate
                          .style -- widget style getter
                          (\w style -> { w | style = style }) -- widget style setter


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of

    Begin ->
      let 
        (widgets, fx) = 
               UI.animate
                 |> UI.duration (0.4*second)
                 |> UI.easing (\x -> (1 - cos (pi * x)) / 2)
                 |> UI.props 
                     [ Left (UI.to 100) Percent
                     ] 
              |> UI.andThen
                 |> UI.duration (0.4*second)
                 |> UI.easing (\x -> (1 - cos (pi * x)) / 2)
                 |> UI.duration (0.4*second)
                 |> UI.props 
                     [ Left (UI.to 0) Percent
                     ] 
                
                 |> forwardToAllWidgets model.widgets
      in
        ( { model | widgets = widgets }
        , fx )

    Animate i action ->
      let
        (widgets, fx) = 
          forwardToWidget i model.widgets action
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
                             , ("margin-top", "40px")
                             , ("margin-left", "auto")
                             , ("margin-right", "auto")
                             , ("padding", "25px")
                             , ("text-align", "center")
                             , ("border-radius", "5px")
                             --, ("background-color", "#AAA")
                             , ("cursor", "pointer")
                            ]
            in
              div [ onClick address Begin
                  , style triggerStyle  
                  ]

                  [ h1 [ style [("padding","0px")]] 
                        [ text "Begin Performance Test"]
                   , div [ style [("position", "relative")]]
                         (List.map (\w -> Html.Lazy.lazy (viewWidget address) w) model.widgets)
                  
                  ] 

viewWidget : Address Action -> Widget -> Html
viewWidget address model =
                let
                  menuStyle =  [ ("border-radius", "5px")
                               , ("width", "10px")
                                , ("height", "10px")
                                , ("background-color", "#111")
                                , ("top", "0px")
                                , ("z-index", "0")
                                , ("display", "inline-block")
                                , ("margin", "10px")
                                , ("text-align", "center")
                                , ("line-height", "10px")
                                , ("position", "absolute")
                                ]
                in
                  div [ style (menuStyle ++ (UI.render model.style)) ]
                      [  ]


init : ( Model, Effects Action )
init = ( { widgets = List.map (\i -> initWidget i) [0..100] 
         }
       , Effects.none )

initWidget i = 
        { style = UI.init 
                      [ Left 0 Percent
                      , Top (i * 12.0) Px
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



