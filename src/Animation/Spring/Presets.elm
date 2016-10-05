module Animation.Spring.Presets exposing (Spring, zippy, wobbly, stiff)

{-|
Spring presets to be used with `Animation.styleWith` and `Animation.toWith`

@docs Spring, zippy, wobbly, stiff

-}


{-|
-}
type alias Spring =
    { stiffness : Float
    , damping : Float
    }


{-| The default used by elm-style-animation.
-}
zippy : Spring
zippy =
    { stiffness = 400
    , damping = 28
    }


{-|
-}
wobbly : Spring
wobbly =
    { stiffness = 180
    , damping = 12
    }


{-|
-}
stiff : Spring
stiff =
    { stiffness = 210
    , damping = 20
    }
