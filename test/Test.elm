module Main (..) where

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
import Easing exposing (easeInBounce, easeInSine, easeOutSine)
import String
import Graphics.Element exposing (Element)
import ElmTest exposing (..)


tests : Test
tests =
  suite
    "HTML Animation Testing Suite"
    [ test "Addition" (assertEqual (3 + 7) 10)
    , test "String.left" (assertEqual "a" (String.left 1 "abcdefg"))
    , test "This test should fail" (assert False)
    ]


type Action
  = NoOp
  | CreateAnimation Int UI.Action
  | StartDelayTest
  | InterruptWithDelay
  | UninitializedPropWarning
  | UninitializedUnitWarning
  | Animate Int UI.Action


type alias Model =
  { widgets : List Widget
  , interruptable : Bool
  }


type alias Widget = 
  { style : UI.Animation 
  }

update : Action -> Model -> ( Model, Effects Action )
update action model =
    case action of
      NoOp ->
        ( model, Effects.none )

      CreateAnimation i anim ->
        let
          ( widgets, fx ) =
            anim |> forwardToWidget i model.widgets
        in
          ( { model
              | widgets = widgets
            }
          , fx
          )

      UninitializedUnitWarning ->
        let
          ( widgets, fx ) =
            UI.animate
              |> UI.props
                  [ Left (UI.to 50) Px 
                  ]
              |> forwardToWidget 0 model.widgets
        in
          ( { model
              | widgets = widgets
            }
          , fx
          )

      UninitializedPropWarning ->
        let
          ( widgets, fx ) =
            UI.animate
              |> UI.props
                  [ Right (UI.to 50) Px 
                  ]
              |> forwardToWidget 0 model.widgets
        in
          ( { model
              | widgets = widgets
            }
          , fx
          )


      StartDelayTest ->
        let
          ( widgets, fx ) =
            UI.animate
              |> UI.duration (5.0 * second)
              |> UI.easing easeInBounce
              |> UI.props
                  [ Left (UI.to 50) Percent
                  ]
              |> forwardToWidget 2 model.widgets
        in
          ( { model
              | widgets = widgets
              , interruptable = True
            }
          ,  fx
          )

      InterruptWithDelay ->
        let
          ( widgets, fx ) =
            UI.animate
              |> UI.delay (1.0 * second)
              |> UI.props
                  [ Left (UI.to 100) Percent
                  ]
              |> forwardToWidget 2 model.widgets
        in
          ( { model | widgets = widgets }
          , fx
          )

      Animate i action ->
        let
          (widgets, fx) = forwardToWidget i model.widgets action
        in
          ( { model | widgets = widgets }
          , fx )


view : Address Action -> Model -> Html
view address model =
  let
    triggerStyle =
      [ ( "position", "relative" )
      , ( "margin", "15px auto" )
      , ( "width", "800px" )
      , ( "padding", "25px;" )
      ]
  in
    div
      [ style triggerStyle
      ]
      [ h1
          []
          [ text "Test Suite" ]
      , fromElement <| elementRunner tests
      , uninitializedPropertyWarning address
      , basic address model
      ]


testStyle =  [("margin-top", "95px")]


basic : Address Action -> Model -> Html
basic address model =
  let
    action =
      if model.interruptable then
        InterruptWithDelay
      else
        StartDelayTest
  in
    div
      [ style
          ([ ( "cursor", "pointer" )
          , ( "width", "100%" )
          
          ] ++ testStyle )
      ]
      [ h1 [] [ text "Animations"]

      , animation address
          "Basic" 
          (CreateAnimation 1
            ( UI.animate
                  |> UI.props
                      [ Left (UI.to 100) Percent
                      ]
            )
          ) 
          (renderWidget 1 model.widgets)

      , animation address
          "Delay and Interrupt" 
          action 
          (renderWidget 2 model.widgets)
      
      ]

animation : Address Action -> String -> Action -> List (String, String) -> Html
animation address label action rendered =
      div [ onClick address action 
          , style [("margin-bottom", "85px")]
          ]
          [ h2 [ style [ ("color", "#CCC")
                       ] 
               ] 
               [ text label ]
          , renderStyleAsText rendered
          , div
              [ style
                  [ ( "height", "0px" )
                  , ( "border-top", "2px dashed #CCC" )
                  , ( "margin-top", "35px")
                  ] 
              ]
              [ div
                  [ style
                      <| [ ( "position", "relative" )
                         , ( "margin-top", "-11px" )
                         ]
                      ++ rendered
                  ]
                  []
              ] 
          ]



renderStyleAsText : List (String, String) -> Html
renderStyleAsText sty = 
                  let
                    renderEl (name, val) = 
                      div [ style <| [ ("font-size", "11px")
                                     , ("margin", "15px")
                                     , ("line-height", "0px")
                                     ]
                          ]
                          [ text (name ++ ": " ++ val) ]
                  in
                    div []
                        (List.map renderEl sty)



uninitializedPropertyWarning : Address Action -> Html
uninitializedPropertyWarning address = 
    div
        [ style
            ([ ( "width", "100%" )
            ] ++ testStyle )
        ]
        [ h1 [] [ text "Warnings" ]
        , p [] [text "Click circle and verify a warning has occurred in the console"]
        , div
            [ onClick address UninitializedUnitWarning
            , style
                [ ("border-radius", "40px")
                , ("width", "80px")
                , ("height", "80px")
                , ("background-color", "#CCC")
                , ("text-align", "center")
                , ("display", "inline-block")
                , ("margin", "10px")
                , ("padding-top", "20px")
                , ("box-sizing", "border-box")
                , ( "cursor", "pointer" )
                ]
            ]
            [text "Wrong Units"]
        , div
            [ onClick address UninitializedPropWarning
            , style
                [ ("border-radius", "40px")
                , ("width", "80px")
                , ("height", "80px")
                , ("background-color", "#CCC")
                , ("display", "inline-block")
                , ("text-align", "center")
                , ("margin", "10px")
                , ("padding-top", "20px")
                , ("box-sizing", "border-box")
                , ( "cursor", "pointer" )
                ]
            ]
            [text "Wrong Property"]
        ]








init : ( Model, Effects Action )
init =
  ( { widgets = List.repeat 5 emptyWidget
       
    , interruptable = False
    }
  , Effects.none
  )

emptyWidget : Widget
emptyWidget =
  { style =  
      UI.init
          [ BorderRadius 10 Px
          , Width 20 Px
          , Height 20 Px
          , BackgroundColor |> UI.rgb 100 100 100
          , Left 0 Percent
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


forwardToWidget = 
    UI.forwardToIndex
        Animate
        .style -- widget style getter
        (\w style -> { w | style = style }) -- widget style setter
                                    



renderWidget : Int -> List Widget -> List (String, String)
renderWidget i widgets = 
                 Maybe.withDefault []
                  <| Maybe.map (\(i, w) -> UI.render w.style)
                  <| List.head
                  <| List.filter (\(j,w) -> j == i) 
                  <| List.indexedMap (,) widgets







