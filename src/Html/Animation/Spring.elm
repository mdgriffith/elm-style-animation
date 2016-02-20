
module Html.Animation.Spring (Model, Physical, update, atRest, duration) where



import Time exposing (Time, second)

--type alias Model =
--  { stiffness : Float
--  , damping : Float
--  , position : Float
--  , velocity : Float
--  , destination : Float
--  }


type alias Model =
  { stiffness : Float
  , damping : Float
  , destination : Float
  }

type alias Physical =
  { position : Float
  , velocity : Float
  }

tolerance = 0.01

vTolerance = 0.1
  --1.0e-2

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
