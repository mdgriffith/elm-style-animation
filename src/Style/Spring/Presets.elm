module Style.Spring.Presets exposing (SpringProps, noWobble, gentle, wobbly, stiff)


{-|

@docs SpringProps, noWobble, gentle, wobbly, stiff

-}

{-| A type for spring properties, stiffness and damping.
-}
type alias SpringProps =
    { stiffness : Float
    , damping : Float
    }


{-| A spring preset.
-}
noWobble : SpringProps
noWobble =
    { stiffness = 170
    , damping = 26
    }


{-| A spring preset.
-}
gentle : SpringProps
gentle =
    { stiffness = 120
    , damping = 14
    }


{-| A spring preset.
-}
wobbly : SpringProps
wobbly =
    { stiffness = 180
    , damping = 12
    }


{-| A spring preset.
-}
stiff : SpringProps
stiff =
    { stiffness = 210
    , damping = 20
    }
