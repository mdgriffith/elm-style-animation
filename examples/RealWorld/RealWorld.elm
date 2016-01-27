
import StartApp exposing (start)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects exposing (Effects, Never)
import Signal exposing (Address)
import Task

import Time exposing (second)

import Html.Animation as UI
import Html.Animation exposing (..)


type alias Model = 
            { submenus : List Submenu
            , style : UI.Animation
            , icon : String
            , open : Bool
            }

type alias Submenu =
            { style : UI.Animation
            , icon : String
            }

type Action = Toggle
            | Animate AnimationTarget UI.Action


type AnimationTarget = OnMenu
                     | OnSubmenu Int


forwardToSubmenu = UI.forwardTo 
                      (\i action -> Animate (OnSubmenu i) action)
                      .style -- widget style getter
                      (\w style -> { w | style = style }) -- widget style setter
                                    
forwardToAllSubmenus = UI.forwardToAll 
                          (\i action -> Animate (OnSubmenu i) action)
                          .style -- widget style getter
                          (\w style -> { w | style = style }) -- widget style setter
                                    




update : Action -> Model -> ( Model, Effects Action )
update action model =
            case action of
              Toggle ->
                if model.open then
                  let
                    (submenus, fx) = 
                        UI.animate 
                          |> UI.props 
                            [ TranslateY (to 0) Px
                            ] 
                        |> forwardToAllSubmenus model.submenus

                  in
                    ( { model | submenus = submenus 
                              , open = False }
                    , fx )
                else
                  let
                    (submenus, fx) = 
                        UI.animate 
                          |> UI.props 
                            [ TranslateY (to 130) Px
                            ] 
                        |> forwardToAllSubmenus model.submenus

                  in
                    ( { model | submenus = submenus 
                              , open = True }
                    , fx )


              Animate target action ->
                case target of
                  OnMenu ->
                    let
                      (anim, fx) = UI.update action model.style
                    in
                      ( { model | style = anim }
                      , Effects.map (Animate OnMenu) fx )

                  OnSubmenu i ->
                    let
                      (submenus, fx) = forwardToSubmenu i model.submenus action
                    in
                      ( { model | submenus = submenus }
                      , fx )


view : Address Action -> Model -> Html
view address model =
                let
                  menuStyle = [ ("width", "60px")
                              , ("height", "60px")
                              , ("position", "relative")
                              , ("top", "300px")
                              , ("margin-left", "auto")
                              , ("margin-right", "auto")
                              ]
                  coverStyle = [ ("border-radius", "30px")
                            , ("width", "60px")
                            , ("height", "60px")
                            , ("position", "absolute")
                            , ("background-color", "red")
                            , ("top", "0px")
                            , ("left", "0px")
                            , ("z-index", "5")
                            , ("cursor", "pointer")
                            , ("text-align", "center")
                            , ("line-height", "60px")
                            , ("font-size", "39px")
                            ] ++ (UI.render model.style)
                in
                  div [ class "menu", style menuStyle ]

                      ([ div [ style coverStyle
                             , onClick address Toggle ] 
                             [ div [ class ("menu-icon fa fa-" ++ model.icon) ] [ text "+"]
                             ] 
                      ] ++ (List.map (viewSubmenu address) model.submenus))
                      



viewSubmenu : Address Action -> Submenu -> Html
viewSubmenu address submenu =
                  let
                    menuStyle = [ ("border-radius", "20px")
                            , ("width", "40px")
                            , ("height", "40px")
                            , ("position", "absolute")
                            , ("background-color", "grey")
                            , ("top", "0px")
                            , ("left", "0px")
                            , ("z-index", "0")
                            , ("display", "inline-block")
                            , ("margin", "10px")
                            , ("text-align", "center")
                            , ("line-height", "40px")
                            ]
                            ++ (UI.render submenu.style)
                  in
                    div [ class ("submenu fa fa-" ++ submenu.icon)
                      , style menuStyle ] 
                      [ text "Hi" ]





init : ( Model, Effects Action )
init = ( { style = UI.init 
                      [ ]
         , icon = "plus"
         , open = False
         , submenus = List.indexedMap (\i x -> createSubmenu x (toFloat i) 8) ["edit", "camera", "pony", "pants", "burrito", "other"
                                                                              , "edit", "camera" ]
         }
       , Effects.none )


createSubmenu : String -> Float -> Float -> Submenu
createSubmenu icon index total = 
                let
                  angle = ((index * 0.1) - ((total-1)*0.05)) + 0.5
                in
                  { icon = icon
                  , style = 
                      UI.init [ Rotate angle Turn
                              , TranslateY 0 Px
                              , Rotate (-1.0*angle) Turn
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