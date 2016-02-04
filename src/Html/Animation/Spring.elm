
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

tolerance =
  1.0e-4

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
    if (abs (spring.destination - newX)) < tolerance && abs newV < tolerance then
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
  physical.position == spring.destination && physical.velocity == 0


duration : Model -> Physical -> Time
duration spring phys =
  snd
    <| List.foldl
        (\t ( phys, d ) ->
          if atRest spring phys then
            ( phys, d )
          else 
           ( update t spring phys, t )
        )
        ( phys, 0 )
        [1..1000]