

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


type alias Model = 
            { widgets : List Widget
            , open : Bool
            }

type alias Widget = 
            { style : UI.Animation 
            } 
-- UPDATE

type Action = Show 
            | Hide
            | Toggle
            | Animate Int UI.Action



onWidget = 
  UI.forwardToIndex
      Animate
      .style -- widget style getter
      (\w style -> { w | style = style }) -- widget style setter
                                    
onAllWidgets = 
    UI.forwardToAll 
        Animate
        .style -- widget style getter
        (\w style -> { w | style = style }) -- widget style setter


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of

    Toggle ->
      if model.open then
        update Hide model
      else
        update Show model

    Show ->
      let 
        (widgets, fx) = 
              UI.stagger
                (\total i -> 
                   UI.animate
                     |> UI.delay (i * 0.05 * second)
                     |> UI.spring UI.wobbly
                     |> UI.props 
                         [ Left (UI.to 100) Px
                         ] 
                )
                |> onAllWidgets model.widgets

      in
        ( { model | widgets = widgets 
                  , open = True }
        , fx )

    Hide ->
      let 
        (widgets, fx) = 
            UI.stagger
              (\total i -> 
                UI.animate
                   |> UI.delay ((i * 0.05) * second)
                   |> UI.spring UI.wobbly
                   |> UI.props 
                       [ Left (UI.to -70) Px
                       ] 
              )
              |> onAllWidgets model.widgets

      in
        ( { model | widgets = widgets
                  , open = False }
        , fx )


    Animate i action ->
      let
        (widgets, fx) = 
            onWidget i model.widgets action
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
              div [ onClick address Toggle
                  , style triggerStyle  
                  ]

                  <| List.concat 
                      [ [ h1 [ style [("padding","25px")]] 
                             [ text "Click me!"]
                        , p [] [ text "This example shows staggered animations"]
                        ]
                      , List.map (viewWidget address) model.widgets
                      ]
    

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
init = ( { widgets = List.map (\i -> initWidget i) [0..10]
         , open = False  
         }
       , Effects.none )

initWidget i = 
        { style = 
            UI.init 
                [ Left -50.0 Px
                , Top (i * 45.0) Px
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



