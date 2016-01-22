

import StartApp exposing (start)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects exposing (Effects, Never)
import Signal exposing (Address)
import Task

import Time exposing (second)

import HtmlAnimation as UI



type alias Model = 
            { menuAnimation : UI.Model 
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
              UI.animateOn model.menuAnimation
                 <| UI.duration (0.4*second)
                 <| UI.props 
                     [ UI.Left UI.Px (UI.to 0) 
                     , UI.Opacity (UI.to 1)
                     ] []

      in
        ( { model | menuAnimation = anim }
        , Effects.map Animate fx )


    Hide ->
      let 
        (anim, fx) = 
            UI.animateOn model.menuAnimation
               <| UI.duration (0.4*second)
               <| UI.props 
                      [ UI.Left UI.Px (UI.to -350) 
                      , UI.Opacity (UI.to 0)
                      ] []
      in
        ( { model | menuAnimation = anim }
        , Effects.map Animate fx )


    Animate action ->
      let
        (anim, fx) = UI.update action model.menuAnimation
      in
        ( { model | menuAnimation = anim }
        , Effects.map Animate fx )



view : Address Action -> Model -> Html
view address model =
            let
              triggerStyle = [ ("position", "absolute")
                             , ("left", "0px")
                             , ("top", "0px")
                             , ("width", "350px")
                             , ("height", "100%")
                             , ("background-color", "#AAA")
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
                                , ("top", "0px")
                                , ("padding", "25px")
                                , ("width", "300px")
                                , ("height", "100%")
                                , ("background-color", "rgb(58,40,69)")
                                , ("color", "white")
                              ]
                in
                  div [ style (menuStyle ++ (UI.render model.menuAnimation)) ]
                      [ h1 [] [ text "Hidden Menu"]
                      , ul [] 
                           [ li [] [text "Some things"]
                           , li [] [text "in a list"]
                            ]
                      ]



init : ( Model, Effects Action )
init = ( { menuAnimation = UI.initStyle [ UI.Left UI.Px -350.0
                                        , UI.Opacity 0.0 
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