



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

import String
import Graphics.Element exposing (Element)

import ElmTest exposing (..)


tests : Test
tests = 
    suite "HTML Animation Testing Suite"
        [ test "Addition" (assertEqual (3 + 7) 10)
        , test "String.left" (assertEqual "a" (String.left 1 "abcdefg"))
        , test "This test should fail" (assert False)
        ]


--main : Element
--main = 
--    elementRunner tests




type Action = NoOp
            | DelayTest
            | AnimateWidget UI.Action 


type alias Model = 
            { widget : UI.Animation
            }





update : Action -> Model -> ( Model, Effects Action )
update action model = 
  case action of
    NoOp ->
      ( model, Effects.none )


    DelayTest ->
      let 
        (anim, fx) = 
              UI.animate 
                 --|> UI.duration (0.4*second)
                 |> UI.props 
                     [ Left (UI.to 50) Percent
                     ] 
              --|> UI.andThen
              --   |> UI.delay (0.5*second)
              --   |> UI.props 
              --       [ Left (UI.to 50) Percent
              --       ] 
                 |> UI.on model.widget
      in
        ( { model | widget = anim }
        , Effects.map AnimateWidget fx )


    AnimateWidget action ->
      let
        (anim, fx) = UI.update action model.widget
      in
        ( { model | widget = anim }
        , Effects.map AnimateWidget fx )



view : Address Action -> Model -> Html
view address model =
            let
              triggerStyle = [ ("position", "relative")
                             , ("margin", "15px auto")
                             , ("width", "800px")
                             , ("padding", "25px;")
                             --, ("height", "100%")
                             --, ("background-color", "#AAA")
                            ]
            in
              div [ style triggerStyle  
                  ]

                  [ h1 [] 
                       [ text "Test Suite"]
                  , fromElement <| elementRunner tests
                  , hr [ style [("height", "1px"), ("border", "none"), ("background-color", "#CCC")]] []
                  , delay address model.widget
                  ]


delay : Address Action -> UI.Animation -> Html
delay address anim = 
        div [ onClick address DelayTest
            , style 
                [ ("cursor", "pointer")
                , ("width", "100%")
               
                --, 
                ]
            ] 
            [ h1 [] [text "Test Delay"]
            , div [ style  
                      [ ("height", "0px")
                      , ("border-top", "2px dashed #CCC")
                      ]
                  ]
                  [ div [ style <| 
                          [ ("position", "relative")
                          , ("margin-top", "-11px")
                          ] ++ (UI.render anim)
                        ] 
                        []
                ]
            ]


init : ( Model, Effects Action )
init = ( { widget = UI.init 
                        [ BorderRadius 10 Px
                        , Width 20 Px 
                        , Height 20 Px
                        , BackgroundColor |> UI.rgb 100 100 100
                        , Left 0 Percent
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