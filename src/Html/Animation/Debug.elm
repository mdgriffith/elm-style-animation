module Html.Animation.Debug (..) where


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Html.Animation as Animation
import Html.Animation.Core as Core
import Html.Animation.Properties exposing (..)
import Html.Animation.Render as Render

{-| A Debugger

-}



--type alias Model =
--  { start : Maybe Time
--  , elapsed : Time
--  , anim : List StyleKeyframe
--  , previous : Style
--  , interruption : List Interruption
--  }


--type alias Interruption =
--  { at : Time
--  , anim : List StyleKeyframe
--  }


--{-| -}
--type Action
--  = Queue (List StyleKeyframe)
--  | Interrupt (List StyleKeyframe)
--  | Tick Time


--{-| Represent a style animation.
--This is a list of StylePropertys, but instead of having a static value like '5',
--it has a function that takes the previous value, the current time, and provides the current value.
---}
--type alias StyleKeyframe =
--  { target : List (StyleProperty (Physics DynamicTarget))
--  , delay : Time
--  }


--{-| Represent a CSS style as a list of style properties with concrete values.
---}
--type alias Style =
--  List (StyleProperty Static)


--type alias DynamicTarget =
--  Float -> Float -> Float

--type alias Static =
--  Float

--type alias Physics a =
--  { target : a
--  , physical : Spring.Physical
--  , spring : Spring.Model
--  , easing : Maybe Easing
--  }


--type alias Easing =
--  { ease : Float -> Float
--  , counterForce : Spring.Model
--  , counterForcePhys : Maybe Spring.Physical
--  , duration : Time
--  }

inset : List (String, String)
inset = 
    [ ("border-radius", "5px")
    , ("padding", "10px")
    , ("background-color", "#EEE")
    ]


--  debug =
--      toIndex 4 
--        (\submenu -> ADebug.view submenu.style)
--        model.submenus


--toIndex i fn xs =
--              List.concat 
--                 <| List.indexedMap 
--                        (\j x -> 
--                          if i == j then
--                            [ fn x ]
--                          else
--                            []
--                        ) xs

view : Animation.Animation -> Html
view (Animation.A model) = 
      div [ class "animation-debug"
          , style [ ("position", "fixed")
                  , ("width", "500px")
                  , ("right", "25px")
                  , ("top", "25px")
                  , ("color", "#111")
                  ] 
          ]
          [ div [ ]
                [ p [ style [("font-weight", "bold"), ("margin-top", "40px")]] [ text "Times"]
                , div [ class "times", style inset ]
                      [ div [] [text ("start: " ++ toString model.start)]
                      , div [] [text ("elapsed: " ++ toString model.elapsed)]
                      ]
                ]
          , div []
                [ p [ style [("font-weight", "bold"), ("margin-top", "40px")]] [ text "Previous Style"]
                , viewStyle model.previous
                ]
          , div []
                [ p [ style [("font-weight", "bold"), ("margin-top", "40px")]] [ text "Interruptions"]
                , viewInterruptions model.interruption
                ]
          , div []
                [ p [ style [("font-weight", "bold"), ("margin-top", "40px")]] [ text "Keyframes"]
                , viewKeyframes model.anim
                ] 
          ]




viewStyle : Core.Style -> Html
viewStyle sty = 
        div [ class "style", style inset ]
            <| List.map 
                (\prop -> 
                    let
                      txt = Render.name prop ++ ": " ++ Render.value prop
                    in
                      div [] [ text txt ]
                ) 
                sty



viewInterruptions : List Core.Interruption -> Html
viewInterruptions ints = 
        div [ class "interruptions", style inset ]
            <| List.map 
                (\int -> 
                    let
                      txt = "Interruption at " ++ toString int.at
                    in
                      div [] [ text txt ]
                ) 
                ints




viewKeyframes : List Core.StyleKeyframe -> Html
viewKeyframes frames = 
        div [ class "keyframes", style inset ]
            <| List.map 
                (\frame -> 
                    let
                      txt = "Keyframe" 
                    in
                      div [] [ text txt ]
                ) 
                frames

