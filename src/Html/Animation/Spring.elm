
module Html.Animation.Spring (Model, Physical, update, atRest, duration) where

import Time exposing (Time, second)
import Debug

{-|  Springs are always modeled from 0-1


-}

type alias Model =
  { stiffness : Float
  , damping : Float
  , destination : Float
  }

type alias Physical =
  { initial : Float
  , position : Float
  , velocity : Float
  }

tolerance = 0.01

vTolerance = 0.1
  --1.0e-2

bounds : ( Float, Float )
bounds = (0 ,1000)

boundRange = snd bounds - fst bounds 


update : Time -> Model -> Physical -> Physical
update dtms spring phys =
          let
            dt =
              dtms / 1000

            fspring =
              -spring.stiffness * (phys.position - spring.destination)

            fdamper =
              -spring.damping * phys.velocity

            a =
              fspring + fdamper

            newV =
              phys.velocity + a * dt

            newX =
              phys.position + newV * dt

          in
            if (abs (spring.destination - newX)) < tolerance && abs newV < vTolerance then
              { phys
                | position = spring.destination
                , velocity = 0.0
              }
            else
              { phys
                | position = newX
                , velocity = newV
              }





atRest : Model -> Physical -> Bool
atRest spring physical =
     abs (spring.destination - physical.position) < tolerance 
  && abs physical.velocity < vTolerance


duration : Model -> Physical -> Time
duration spring phys =
  snd
    <| List.foldl
        (\t ( phys, d ) ->
          if atRest spring phys then
            ( phys, d )
          else 
           ( update 1 spring phys, t )
        )
        ( phys, 0 )
        [1..10000] -- Ticks in ms



--normalizedUpdate : Time -> Model -> Physical -> Physical
--normalizedUpdate dtms spring phys =
--  let
--    range = (spring.destination - phys.initial)

--    normalizedPosition = ((phys.position - phys.initial) / range) * boundRange

--    normalizedPhys =
--        { position = normalizedPosition
--        , initial = fst bounds
--        , velocity = (phys.velocity / range) * boundRange
--        }

--    normalizedSpring =
--        { spring 
--            | destination = snd bounds
--        }

--    updated = 
--        internalUpdate dtms normalizedSpring normalizedPhys

--  in
--    { phys 
--        | velocity = ((updated.velocity / boundRange) * range)
--        , position = ((updated.position / boundRange) * range) + phys.initial
--    }






--x = Debug.log "no wobble duration" 
--            <| Spring.duration { noWobble | destination = 1 }
--                        { position = 0
--                        , velocity = 0
--                        }

--y = Debug.log "gentle duration" 
--            <| Spring.duration { gentle | destination = 1 }
--                        { position = 0
--                        , velocity = 0
--                        }

--z = Debug.log "stiff duration" 
--            <| Spring.duration { stiff | destination = 1 }
--                        { position = 0
--                        , velocity = 0
--                    }

--a = Debug.log "wobbly duration" 
--            <| Spring.duration { wobbly | destination = 1 }
--                        { position = 0
--                        , velocity = 0
--                    }

--b = Debug.log "fast and loose duration" 
--            <| Spring.duration { fastAndLoose | destination = 1 }
--                        { position = 0
--                        , velocity = 0
--                    }
