module ElmUI where


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, second)
import Effects exposing (Effects)

import Signal exposing (Address)

import Animation exposing (..)

import Debug





type alias Model = { start : Maybe Time 
                   , elapsed : Time
                   , anim : Maybe AnimationState
                   , duration : Time
                   }




type AnimationState = 
              FadeIn Time

empty : Model
empty = { elapsed = 1.0*second
        , start = Nothing
        , anim = Nothing
        , duration = 2.0*second
        }


type Action 
        = Begin AnimationState
        | Tick Time


render : Model -> List (String, String)
render model = 
            case model.anim of
              Nothing -> []
              Just anim ->
                case anim of
                  FadeIn dur ->
                    fadeIn dur model.elapsed
                    --anim model.elapsed


update : Action -> Model -> ( Model, Effects Action )
update action model = 
        case action of

          Begin anim ->
              ( { model | anim = Just anim
                        , elapsed = 0.0
                        , start = Nothing }
              , Effects.tick Tick )

          Tick now ->
            let
              start = 
                case model.start of
                  Nothing -> now
                  Just t -> t
              newElapsed = now - start
            in
              if newElapsed >= model.duration then
                ( { model | elapsed = 0.0
                          , anim = Nothing
                          , start = Nothing }
                , Effects.none )
              else
                ( { model | elapsed = newElapsed 
                          , start = Just start }
                , Effects.tick Tick )






fadeIn : Time -> Time -> List (String, String)
fadeIn dur current = opacity 
                       (animation 0 
                            |> from 0.0 
                            |> to 1.0
                            |> duration dur) current


opacity : Animation -> Time -> List (String, String)
opacity anim now = [("opacity", (toString (animate now anim)))]
