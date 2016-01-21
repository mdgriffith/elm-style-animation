module Multi where

import Effects exposing (Effects)
import List
import Dict 
import ElmUI



type alias Model comparable = 
             Dict.Dict comparable ElmUI.Model

type Action comparable = 
       Dispatch comparable ElmUI.Action


render : comparable -> Dict.Dict comparable ElmUI.Model -> List (String, String)
render i model =
            let
              style = Dict.get i model
            in
              case style of
                Nothing -> []
                Just s ->
                  ElmUI.render s
             


animate : comparable ->  Dict.Dict comparable ElmUI.Model -> ElmUI.DynamicStyleAnimation -> ( Dict.Dict comparable ElmUI.Model, Effects (Action comparable) )
animate id model anims = update (Dispatch id (ElmUI.Begin anims)) model




--animate : Model -> DynamicStyleAnimation -> ( Model, Effects Action )
--animate model anims = update (Begin anims) model





update : Action comparable -> Dict.Dict comparable ElmUI.Model -> ( Dict.Dict comparable ElmUI.Model , Effects (Action comparable))
update action model =
        case action of

          Dispatch id uiAction ->
              case Dict.get id model of
                Nothing -> ( model, Effects.none )
                Just style ->
                  let
                    (updatedStyle, fx) = ElmUI.update uiAction style
                  in
                    ( Dict.insert id updatedStyle model
                    , Effects.map (Dispatch id) fx )

            
        