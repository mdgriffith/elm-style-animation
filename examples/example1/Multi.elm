module Multi where

import Effects exposing (Effects)
import List
import ElmUI


type alias Model = List (Int, ElmUI.Model)

type Action = Dispatch Int ElmUI.Action


render : Int -> Model -> List (String, String)
render i model =
              List.concatMap 
                  (\x -> 
                    if fst x == i then 
                      ElmUI.render (snd x)
                    else [] ) model


animate : Int -> ElmUI.DynamicStyleAnimation -> Model -> ( Model, Effects Action )
animate id anims model = update (Dispatch id (ElmUI.Begin anims)) model


update : Action -> Model -> ( Model, Effects Action )
update action model =
        case action of
          Dispatch id uiAction ->
            let
              updated = 
                List.map 
                      (\(i, ui) ->
                          if i == id then
                            let
                              (ui2, fx) = ElmUI.update uiAction ui
                            in
                              ((i, ui2), [(id, fx)])
                          else
                            ((i, ui), [])
                        ) model
              (fxID, fx) = Maybe.withDefault (0, Effects.none)
                        <| List.head 
                        <| List.concatMap snd updated
            in
              ( List.map fst updated
              , Effects.map (Dispatch fxID) fx
              )