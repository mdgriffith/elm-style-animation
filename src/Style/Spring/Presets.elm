module Style.Spring.Presets exposing (SpringProps, noWobble, gentle, wobbly, stiff) --where


type alias SpringProps =
    { stiffness : Float
    , damping : Float
    }


{-| A spring preset.  Probably should be your initial goto for using springs.
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