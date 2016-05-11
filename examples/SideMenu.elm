import Time exposing (second)
import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Animation as UI
import Html.Animation.Properties exposing (..)
import AnimationFrame


type alias Model = 
            { style : UI.Animation 
            }

type Action = Show 
            | Hide
            | Animate Float


styles = 
  { open = 
      [ Left 0.0 Px
      , Opacity  1.0
      ] 
  , closed = 
      [ Left -350.0 Px
      , Opacity 0.0
      ] 
  }



update : Action -> Model -> ( Model, Cmd Action )
update action model =
  case action of
    Show ->
      ( { model
            | style =  
                UI.animateTo 
                    styles.open
                    model.style
      }
      , Cmd.none
      )

    Hide ->
        ( { model
              | style =  
                  UI.animateTo
                     styles.closed
                     model.style 
          }
        , Cmd.none
        )

 
    Animate time ->
      ( { model 
            | style = UI.tick time model.style 
        }
      , Cmd.none)


view :  Model -> Html Action
view model =
    div [ onMouseEnter Show
        , onMouseLeave Hide
        , style [ ("position", "absolute")
                 , ("left", "0px")
                 , ("top", "0px")
                 , ("width", "350px")
                 , ("height", "100%")
                 , ("border", "2px dashed #AAA")
                ]
        ]
        [ h1 [ style [("padding","25px")]] 
             [ text "Hover here to see menu!"]
        , div [ style ([ ("position", "absolute")
                      , ("top", "-2px")
                      , ("margin-left", "-2px")
                      , ("padding", "25px")
                      , ("width", "300px")
                      , ("height", "100%")
                      , ("background-color", "rgb(58,40,69)")
                      , ("color", "white")
                      , ("border", "2px solid rgb(58,40,69)")  
                    ] ++ (UI.render model.style)) 
              ]
              [ h1 [] [ text "Hidden Menu"]
              , ul [] 
                   [ li [] [text "Some things"]
                   , li [] [text "in a list"]
                   ]
              ]
        ]

subscriptions : Model -> Sub Action
subscriptions model =
  AnimationFrame.times Animate



init : ( Model, Cmd Action )
init = ( { style = UI.init styles.closed }
       , Cmd.none )



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

