

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



type alias Model = 
            { style : UI.Animation 
            }

-- UPDATE

type Action = Show 
            | Hide
            | Animate UI.Action



update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Show ->
      let 
        (anim, fx) = 
              UI.animate 
                 --|> UI.duration (0.4*second)
                 |> UI.props 
                     [ Left (UI.to 0) Px
                     , Opacity (UI.to 1)
                     ] 
                 |> UI.on model.style

      in
        ( { model | style = anim }
        , Effects.map Animate fx )


    Hide ->
      let 
        (anim, fx) = 
            UI.animate
               --|> UI.duration (0.4*second)
               |> UI.props 
                      [ Left (UI.to -350) Px
                      , Opacity (UI.to 0)
                      ] 
               |> UI.on model.style
      in
        ( { model | style = anim }
        , Effects.map Animate fx )


    Animate action ->
      let
        (anim, fx) = UI.update action model.style
      in
        ( { model | style = anim }
        , Effects.map Animate fx )



view : Address Action -> Model -> Html
view address model =
            let
              triggerStyle = [ ("position", "absolute")
                             , ("left", "0px")
                             , ("top", "0px")
                             , ("width", "350px")
                             , ("height", "100%")
                             --, ("background-color", "#AAA")
                             , ("border", "2px dashed #AAA")
                            ]
            in
              div [ onMouseEnter address Show
                  , onMouseLeave address Hide
                  , style triggerStyle  
                  ]

                  [ h1 [ style [("padding","25px")]] 
                       [ text "Hover here to see menu!"]
                  , viewMenu address model 
                  ]

viewMenu : Address Action -> Model -> Html
viewMenu address model =
                let
                  menuStyle = [ ("position", "absolute")
                                , ("top", "-2px")
                                , ("margin-left", "-2px")
                                , ("padding", "25px")
                                , ("width", "300px")
                                , ("height", "100%")
                                , ("background-color", "rgb(58,40,69)")
                                , ("color", "white")
                                , ("border", "2px solid rgb(58,40,69)")
                              ]
                in
                  div [ style (menuStyle ++ (UI.render model.style)) ]
                      [ h1 [] [ text "Hidden Menu"]
                      , ul [] 
                           [ li [] [text "Some things"]
                           , li [] [text "in a list"]
                            ]
                      ]



init : ( Model, Effects Action )
init = ( { style = UI.init 
                      [ Left -350.0 Px
                      , Opacity 0.0 
                      ]
         }
       , Effects.none )

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