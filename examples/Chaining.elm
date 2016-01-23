

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
            { style : UI.StyleAnimation 
            }

-- UPDATE

type Action = ChangeColor
            | Animate UI.StyleAction


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    ChangeColor ->
      let 
        (anim, fx) = 
            UI.animateOn model.style
                    <| UI.props 
                        [ UI.BackgroundColorA 
                              UI.RGBA (UI.to 100) (UI.to 100) (UI.to 100) (UI.to 1.0)  
                        ] 
                <| UI.andThen -- create a new keyframe
                    <| UI.duration (1*second)
                    <| UI.props 
                        [ UI.BackgroundColorA 
                              UI.RGBA (UI.to 178) (UI.to 201) (UI.to 14) (UI.to 1.0) 
                        ] 
                <| UI.andThen 
                    <| UI.props 
                        [ UI.BackgroundColorA 
                              UI.RGBA (UI.to 58) (UI.to 40) (UI.to 69) (UI.to 1.0) 
                        ] 
                <| [] 

      in
        ( { model | style = anim }
        , Effects.map Animate fx )



    Animate action ->
      let
        (anim, fx) = UI.updateStyle action model.style
      in
        ( { model | style = anim }
        , Effects.map Animate fx )



view : Address Action -> Model -> Html
view address model =
            let
              triggerStyle = [ ("position", "relative")
                             , ("margin", "200px auto")
                             , ("width", "250px")
                             , ("height", "250px")
                             , ("text-align","center")
                             , ("line-height", "250px")
                             , ("color", "white")
                             ]
            in
              div [ onClick address ChangeColor
                  , style (triggerStyle ++ UI.render model.style)
                  ]

                  [ text "Click to Change Color" ]




init : ( Model, Effects Action )
init = ( { style = 
              UI.initStyle 
                  [ UI.BackgroundColorA UI.RGBA 58 40 69 1.0 ]
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