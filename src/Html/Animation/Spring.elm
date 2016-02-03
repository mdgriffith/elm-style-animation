
module Html.Animation.Spring (Model, Properties, update, atRest, duration) where



import Time exposing (Time, second)

type alias Model =
  { stiffness : Float
  , damping : Float
  , position : Float
  , velocity : Float
  , destination : Float
  }


type alias Properties =
  { stiffness : Float
  , damping : Float
  }

tolerance =
  1.0e-4

update : Time -> Model -> Model
update dtms spring =
  let

    dt =
      dtms / 1000

    fspring =
      -spring.stiffness * (spring.position - spring.destination)

    fdamper =
      -spring.damping * spring.velocity

    a =
      fspring + fdamper

    newV =
      spring.velocity + a * dt

    newX =
      spring.position + newV * dt

  in
    if (abs (spring.destination - newX)) < tolerance && abs newV < tolerance then
      { spring
        | position = spring.destination
        , velocity = 0.0
      }
    else
      { spring
        | position = newX
        , velocity = newV
      }


atRest : Model -> Bool
atRest spring =
  spring.position == spring.destination && spring.velocity == 0


duration : Model -> Time
duration spring =
  snd
    <| List.foldl
        (\t ( spg, d ) ->
          if atRest spg then
            ( spg, d )
          else
            ( update t spg, t )
        )
        ( spring, 0 )
        [1..1000]