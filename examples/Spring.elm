

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
            { style : UI.Animation 
            , left : Bool
            }

-- UPDATE

type Action = Toggle
            | ToRight
            | ToLeft
            | Animate UI.Action



update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of

    Toggle ->
      if model.left then
        update ToRight model
      else
        update ToLeft model


    ToRight ->
      let 
        (anim, fx) = 
              UI.animate 
                 --|> UI.duration (0.4*second)
                 |> UI.spring UI.wobbly
                 |> UI.props 
                     [ UI.Left (UI.to 80) UI.Percent
                     ] 
                 |> UI.on model.style

      in
        ( { model | style = anim
                  , left = False }
        , Effects.map Animate fx )


    ToLeft ->
      let 
        (anim, fx) = 
            UI.animate
               --|> UI.duration (0.4*second)
               |> UI.spring UI.wobbly
               |> UI.props 
                      [ UI.Left (UI.to 20) UI.Percent
                      ] 
               |> UI.on model.style
      in
        ( { model | style = anim 
                  , left = True }
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
              triggerStyle = [ ("position", "fixed")
                             , ("left", "-150px")
                             , ("top", "0px")
                             , ("width", "300px")
                             , ("margin-top", "250px")
                             , ("margin-left", "50%")
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

                  [ h1 [ style [("padding","25px")]] 
                       [ text "Click Here to Move Dot"]
                  , viewMenu address model 
                  ]

viewMenu : Address Action -> Model -> Html
viewMenu address model =
                let
                  menuStyle = [ ("position", "fixed")
                                , ("top", "450px")
                                , ("padding", "25px")
                                , ("width", "50px")
                                , ("height", "50px")
                                , ("margin-left", "-25px")
                                , ("background-color", "rgb(58,40,69)")
                                , ("color", "white")
                                , ("border-radius", "50px")
                              ]
                in
                  div [ style (menuStyle ++ (UI.render model.style)) ]
                      [ 
                      ]



init : ( Model, Effects Action )
init = ( { style = UI.init 
                      [ UI.Left 20.0 UI.Percent
                      ]
         , left = True
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