

import StartApp exposing (start)

import Effects exposing (Effects, Never)
import Task
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict

import Time exposing (Time, second)
import Signal exposing (Address)

import ElmUI as UI
--import Multi

-- MODEL

type alias Model = { widgets : List Widget }


type alias Widget = 
          { label : String
          , style : UI.Model
          , action : (Int -> Action)
          }


-- UPDATE

type Action = Rotate Int
            | ChangeBGColor Int
            | Animate Int UI.Action




forwardTo : Int -> List Widget -> (Widget -> (Widget, (Effects UI.Action))) -> (List Widget, Effects UI.Action)
forwardTo i widgets fn = 
              let
                applied = 
                  List.indexedMap 
                        (\j w -> 
                            if j == i then
                              fn w
                            else
                              (w, Effects.none)
                        ) widgets

                combineEffects ef1 ef2 =
                          if ef1 == Effects.none then
                            ef2
                          else
                            ef1
                 
              in
                 List.foldl 
                      (\x acc ->
                          case acc of
                            (ws, eff1) ->
                              case x of
                                (w, eff2) ->
                                  (w::ws, combineEffects eff1 eff2)
                      ) ([], Effects.none) applied



update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Rotate i ->
      let 
        --(anim, fx) = Multi.animate i model.animations
        --                    <| UI.props 
        --                        [ UI.Rotate UI.Turn (UI.add 1) ]

        (widgets, fx) = forwardTo i model.widgets
                      (\w  ->
                        let
                          (newStyle, fx) = 
                              UI.animate w.style
                                    <| UI.props 
                                      [ UI.Rotate UI.Turn (UI.add 1) ]
                        in
                          ({ w | style = newStyle }, fx)
                      )
                          
                                  
      in
        ( { model | widgets = widgets }
        , Effects.map (Animate i) fx )


    ChangeBGColor i ->
       let 
          (widgets, fx) = forwardTo i model.widgets
                        (\w  ->
                          let
                            (newStyle, fx) = 
                                UI.animate w.style
                                      <| UI.props 
                                        [ UI.BackgroundColor UI.RGB (UI.to 255) (UI.to 255) (UI.to 255)]
                          in
                            ({ w | style = newStyle }, fx)
                        )
                            
                                    
        in
          ( { model | widgets = widgets }
          , Effects.map (Animate i) fx )


    Animate i action ->
      let
        (widgets, fx) = forwardTo i model.widgets 
                        (\w -> 
                            let
                             (newStyle, fx) = UI.update action w.style
                            in
                              ( { w | style = newStyle }, fx)
                        )
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

                  --[ box address model (Rotate 1) 1
                  --, box address model Blink 2
                  --, box address model (Rotate 3) 3
                  --]


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




initialWidgetStyle = UI.style 
                        [ UI.Rotate UI.Turn 0.0
                        , UI.RotateY UI.Turn 0.0
                        , UI.Opacity 1
                        , UI.BackgroundColor UI.RGB 58 40 69
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
          --, 
          --    { label = "Change Background Color"
          --    , style = initialWidgetStyle
          --    , action = ChangeBGColor
          --    }
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