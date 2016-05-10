

--import StartApp exposing (start)
--import Effects exposing (Never)
import Html.App as Html
--import Signal exposing (Address)
import Task
import Time exposing (second)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Animation as UI
import Html.Animation.Properties exposing (..)



type alias Model = 
            { style : UI.Animation 
            }

-- UPDATE

type Action = Show 
            | Hide
            | Animate Float




--styles = 
  



{-| Prepare a helper function manage effects and assign styles -}
--onMenu =
--  UI.forwardTo 
--      Animate
--      .style
--      (\w style -> { w | style = style }) -- style setter 


update : Action -> Model -> ( Model, Cmd Action )
update action model =
  case action of
    Show ->
      (model, Cmd.none)
    --   let
    --      (newStyle, fx) = 
    --        UI.animate
    --           |> UI.props 
    --                  [ Left (UI.to 0) Px
    --                  , Opacity (UI.to 1)
    --                  ] 
    --           |> UI.on model.style
    --    in
    --      ( {model| style = newStyle}
    --      , Cmd.none
    --      )

    Hide ->
      (model, Cmd.none)
      --let
      --  (newStyle, fx) = 
      --    UI.animate
      --       |> UI.props 
      --              [ Left (UI.to -350) Px
      --              , Opacity (UI.to 0)
      --              ] 
      --       |> UI.on model.style
      --in
      --  ( {model| style = newStyle}
      --  , Cmd.none
      --  )

 
    Animate time ->
      let
        _ = Debug.log "test" (toString time)
      in
        (model, Cmd.none)
      --let
      --  (newModel, fx) = onMenu model action
      --in
      --  (newModel, Cmd.none)


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
                      ])
                    --] ++ (UI.render model.style)) 
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
  Time.every second Animate



init : ( Model, Cmd Action )
init = ( { style = UI.init 
                      [ Left -350.0 Px
                      , Opacity 0.0 
                      ]
         }
       , Cmd.none )



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

