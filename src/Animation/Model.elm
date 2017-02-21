module Animation.Model exposing (..)

import Time exposing (Time)
import Task


type Animation msg
    = Animation
        { steps : List (Step msg)
        , style : List Property
        , timing : Timing
        , running : Bool
        , interruption : List ( Time, List (Step msg) )
        }


type alias Timing =
    { current : Time
    , dt : Time
    }


type Tick
    = Tick Time


{-|

-}
type Step msg
    = Step
    | To (List Property)
    | ToWith (List Property)
    | Set (List Property)
    | Wait Time
    | Send msg
    | Repeat Int (List (Step msg))
    | Loop (List (Step msg))


type Interpolation
    = Spring
        { stiffness : Float
        , damping : Float
        }
    | Easing
        { progress : Float
        , duration : Time
        , start : Time
        , ease : Float -> Float
        }
    | AtSpeed { perSecond : Float }


{-| For each 'value' of a property, we track position, velocity, interpolation, and target.
-}
type Property
    = ExactProperty String String
    | ColorProperty String Motion Motion Motion Motion
    | ShadowProperty String Bool ShadowMotion
    | Property String Motion
    | Property2 String Motion Motion
    | Property3 String Motion Motion Motion
    | Property4 String Motion Motion Motion Motion
    | AngleProperty String Motion
    | Points (List ( Motion, Motion ))
    | Path (List PathCommand)


type alias Motion =
    { position : Float
    , velocity : Float
    , target : Float
    , interpolation : Interpolation
    , unit : String
    , interpolationOverride : Maybe Interpolation
    }


type alias ShadowMotion =
    { offsetX : Motion
    , offsetY : Motion
    , size : Motion
    , blur : Motion
    , red : Motion
    , green : Motion
    , blue : Motion
    , alpha : Motion
    }


{-| Describe a path.  To be used in conjunction with the 'd' property for styling svg.

`To` versions of the commands are absolute, while others are relative.

-}
type PathCommand
    = Move Motion Motion
    | MoveTo Motion Motion
    | Line Motion Motion
    | LineTo Motion Motion
    | Horizontal Motion
    | HorizontalTo Motion
    | Vertical Motion
    | VerticalTo Motion
    | Curve CubicCurveMotion
    | CurveTo CubicCurveMotion
    | Quadratic QuadraticCurveMotion
    | QuadraticTo QuadraticCurveMotion
    | SmoothQuadratic (List ( Motion, Motion ))
    | SmoothQuadraticTo (List ( Motion, Motion ))
    | Smooth (List ( Motion, Motion ))
    | SmoothTo (List ( Motion, Motion ))
    | ClockwiseArc ArcMotion
    | AntiClockwiseArc ArcMotion
    | Close


type alias CubicCurveMotion =
    { control1 : ( Motion, Motion )
    , control2 : ( Motion, Motion )
    , point : ( Motion, Motion )
    }


type alias QuadraticCurveMotion =
    { control : ( Motion, Motion )
    , point : ( Motion, Motion )
    }


type alias ArcMotion =
    { x : Motion
    , y : Motion
    , radius : Motion
    , startAngle : Motion
    , endAngle : Motion
    }


propertyName : Property -> String
propertyName prop =
    case prop of
        ExactProperty name _ ->
            name

        ColorProperty name _ _ _ _ ->
            name

        ShadowProperty name _ _ ->
            name

        Property name _ ->
            name

        Property2 name _ _ ->
            name

        Property3 name _ _ _ ->
            name

        Property4 name _ _ _ _ ->
            name

        AngleProperty name _ ->
            name

        Points _ ->
            "points"

        Path _ ->
            "path"


mapToMotion : (Motion -> Motion) -> Property -> Property
mapToMotion fn prop =
    case prop of
        ExactProperty name value ->
            ExactProperty name value

        ColorProperty name m1 m2 m3 m4 ->
            ColorProperty name
                (fn m1)
                (fn m2)
                (fn m3)
                (fn m4)

        ShadowProperty name inset shadow ->
            let
                offsetX =
                    shadow.offsetX

                offsetY =
                    shadow.offsetY

                size =
                    shadow.size

                blur =
                    shadow.blur

                red =
                    shadow.red

                green =
                    shadow.green

                blue =
                    shadow.blue

                alpha =
                    shadow.alpha
            in
                ShadowProperty
                    name
                    inset
                    { offsetX = fn offsetX
                    , offsetY = fn offsetY
                    , size = fn size
                    , blur = fn blur
                    , red = fn red
                    , green = fn green
                    , blue = fn blue
                    , alpha = fn alpha
                    }

        Property name m1 ->
            Property name
                (fn m1)

        Property2 name m1 m2 ->
            Property2 name
                (fn m1)
                (fn m2)

        Property3 name m1 m2 m3 ->
            Property3 name
                (fn m1)
                (fn m2)
                (fn m3)

        Property4 name m1 m2 m3 m4 ->
            Property4 name
                (fn m1)
                (fn m2)
                (fn m3)
                (fn m4)

        AngleProperty name m1 ->
            AngleProperty name
                (fn m1)

        Points ms ->
            Points <|
                List.map
                    (\( x, y ) ->
                        ( fn x
                        , fn y
                        )
                    )
                    ms

        Path cmds ->
            Path <|
                List.map
                    (mapPathMotion fn)
                    cmds


mapPathMotion : (Motion -> Motion) -> PathCommand -> PathCommand
mapPathMotion fn cmd =
    let
        mapCoords coords =
            List.map
                (\( x, y ) ->
                    ( fn x
                    , fn y
                    )
                )
                coords
    in
        case cmd of
            Move m1 m2 ->
                Move
                    (fn m1)
                    (fn m2)

            MoveTo m1 m2 ->
                MoveTo
                    (fn m1)
                    (fn m2)

            Line m1 m2 ->
                Line
                    (fn m1)
                    (fn m2)

            LineTo m1 m2 ->
                LineTo
                    (fn m1)
                    (fn m2)

            Horizontal motion ->
                Horizontal
                    (fn motion)

            HorizontalTo motion ->
                HorizontalTo
                    (fn motion)

            Vertical motion ->
                Vertical
                    (fn motion)

            VerticalTo motion ->
                VerticalTo
                    (fn motion)

            Curve { control1, control2, point } ->
                Curve
                    { control1 =
                        ( fn (Tuple.first control1)
                        , fn (Tuple.second control1)
                        )
                    , control2 =
                        ( fn (Tuple.first control2)
                        , fn (Tuple.second control2)
                        )
                    , point =
                        ( fn (Tuple.first point)
                        , fn (Tuple.second point)
                        )
                    }

            CurveTo { control1, control2, point } ->
                CurveTo
                    { control1 =
                        ( fn (Tuple.first control1)
                        , fn (Tuple.second control1)
                        )
                    , control2 =
                        ( fn (Tuple.first control2)
                        , fn (Tuple.second control2)
                        )
                    , point =
                        ( fn (Tuple.first point)
                        , fn (Tuple.second point)
                        )
                    }

            Quadratic { control, point } ->
                Quadratic
                    { control =
                        ( fn (Tuple.first control)
                        , fn (Tuple.second control)
                        )
                    , point =
                        ( fn (Tuple.first point)
                        , fn (Tuple.second point)
                        )
                    }

            QuadraticTo { control, point } ->
                QuadraticTo
                    { control =
                        ( fn (Tuple.first control)
                        , fn (Tuple.second control)
                        )
                    , point =
                        ( fn (Tuple.first point)
                        , fn (Tuple.second point)
                        )
                    }

            SmoothQuadratic coords ->
                SmoothQuadratic <| mapCoords coords

            SmoothQuadraticTo coords ->
                SmoothQuadraticTo <| mapCoords coords

            Smooth coords ->
                Smooth <| mapCoords coords

            SmoothTo coords ->
                SmoothTo <| mapCoords coords

            ClockwiseArc arc ->
                ClockwiseArc <|
                    let
                        x =
                            arc.x

                        y =
                            arc.y

                        radius =
                            arc.radius

                        startAngle =
                            arc.startAngle

                        endAngle =
                            arc.endAngle
                    in
                        { arc
                            | x = fn x
                            , y = fn y
                            , radius = fn radius
                            , startAngle = fn startAngle
                            , endAngle = fn endAngle
                        }

            AntiClockwiseArc arc ->
                AntiClockwiseArc <|
                    let
                        x =
                            arc.x

                        y =
                            arc.y

                        radius =
                            arc.radius

                        startAngle =
                            arc.startAngle

                        endAngle =
                            arc.endAngle
                    in
                        { arc
                            | x = fn x
                            , y = fn y
                            , radius = fn radius
                            , startAngle = fn startAngle
                            , endAngle = fn endAngle
                        }

            Close ->
                Close


updateAnimation : Tick -> Animation msg -> ( Animation msg, Cmd msg )
updateAnimation (Tick now) (Animation model) =
    let
        -- set current and dt time
        timing =
            refreshTiming now model.timing

        -- Resolve potential interrutions
        ( readyInterruption, queuedInterruptions ) =
            List.map
                (\( wait, steps ) ->
                    ( wait - timing.dt, steps )
                )
                model.interruption
                |> List.partition
                    (\( wait, steps ) -> wait <= 0)

        -- if there is more than one matching interruptions,
        -- we only take the first, which is the one that was most recently assigned.
        -- If an interruption does occur, we need to clear any interpolation overrides.
        ( steps, style ) =
            case List.head readyInterruption of
                Just ( wait, interrupt ) ->
                    ( interrupt
                    , List.map (mapToMotion (\m -> { m | interpolationOverride = Nothing })) model.style
                    )

                Nothing ->
                    ( model.steps, model.style )

        ( revisedStyle, sentMessages, revisedSteps ) =
            resolveSteps style steps timing.dt
    in
        ( Animation
            { model
                | timing = timing
                , interruption = queuedInterruptions
                , running =
                    List.length revisedSteps
                        /= 0
                        || List.length queuedInterruptions
                        /= 0
                , steps = revisedSteps
                , style = revisedStyle
            }
        , Cmd.batch <| List.map (\m -> Task.perform identity (Task.succeed m)) sentMessages
        )


refreshTiming : Time -> Timing -> Timing
refreshTiming now timing =
    let
        dt =
            now - timing.current

        -- dt is set to one frame (16.66) if it is a large dt(more than 2 frames),
        -- A large dt means one of the following:
        --    * the user left the browser window and came back
        --    * the animation subscription has stopped calling for updates for a while and started running again
        --
        -- We also have a special case when timing.current == 0, which is happens at startup.
    in
        { current = now
        , dt =
            if dt > 34 || timing.current == 0 then
                16.666
            else
                dt
        }


resolveSteps : List Property -> List (Step msg) -> Time -> ( List Property, List msg, List (Step msg) )
resolveSteps currentStyle steps dt =
    case List.head steps of
        Nothing ->
            ( currentStyle, [], [] )

        Just currentStep ->
            case currentStep of
                Wait n ->
                    if n <= 0 then
                        resolveSteps currentStyle (List.drop 1 steps) dt
                    else
                        -- What about a slight overage of time?
                        ( currentStyle, [], (Wait <| n - dt) :: List.drop 1 steps )

                Send msg ->
                    let
                        ( newStyle, msgs, remainingSteps ) =
                            resolveSteps currentStyle (List.drop 1 steps) dt
                    in
                        ( newStyle, msg :: msgs, remainingSteps )

                To target ->
                    -- Add starting time to any properties with duration/easing
                    -- The boolean is to override interpolation or not
                    if alreadyThere currentStyle target then
                        ( currentStyle
                        , []
                        , List.drop 1 steps
                        )
                    else
                        resolveSteps
                            (startTowards False currentStyle target)
                            (Step :: List.drop 1 steps)
                            dt

                ToWith target ->
                    if alreadyThere currentStyle target then
                        ( currentStyle
                        , []
                        , List.drop 1 steps
                        )
                    else
                        -- Add starting time to any properties with duration/easing
                        -- The boolean is to override interpolation or not
                        resolveSteps
                            (startTowards True currentStyle target)
                            (Step :: List.drop 1 steps)
                            dt

                Set props ->
                    resolveSteps
                        (replaceProps currentStyle props)
                        (List.drop 1 steps)
                        dt

                Step ->
                    let
                        stepped =
                            step dt currentStyle
                    in
                        if List.all isDone stepped then
                            ( List.map (mapToMotion (\m -> { m | interpolationOverride = Nothing })) stepped
                            , []
                            , List.drop 1 steps
                            )
                        else
                            ( stepped
                            , []
                            , steps
                            )

                Loop substeps ->
                    resolveSteps
                        currentStyle
                        (substeps ++ [ Loop substeps ])
                        dt

                Repeat n substeps ->
                    if n <= 0 then
                        resolveSteps currentStyle (List.drop 1 steps) dt
                    else
                        resolveSteps
                            currentStyle
                            (substeps ++ [ Repeat (n - 1) substeps ] ++ List.drop 1 steps)
                            dt


alreadyThere : List Property -> List Property -> Bool
alreadyThere current target =
    startTowards False current target
        |> step 0
        |> List.all isDone


{-|
-}
replaceProps : List Property -> List Property -> List Property
replaceProps props replacements =
    let
        replacementNames =
            List.map propertyName replacements

        removed =
            List.filter (\prop -> not <| List.member (propertyName prop) replacementNames) props
    in
        removed ++ replacements


{-| Property is done?

TODO: What about interlaced property animations?

-}
isDone : Property -> Bool
isDone property =
    let
        motionDone motion =
            let
                runningInterpolation =
                    Maybe.withDefault
                        motion.interpolation
                        motion.interpolationOverride
            in
                case runningInterpolation of
                    Spring _ ->
                        motion.velocity == 0 && motion.position == motion.target

                    Easing eased ->
                        eased.progress == 1 || (eased.progress == 0 && motion.position == motion.target)

                    AtSpeed speed ->
                        motion.position == motion.target
    in
        case property of
            ExactProperty _ _ ->
                True

            ColorProperty _ m1 m2 m3 m4 ->
                List.all motionDone [ m1, m2, m3, m4 ]

            ShadowProperty _ _ shadow ->
                List.all
                    motionDone
                    [ shadow.offsetX
                    , shadow.offsetY
                    , shadow.size
                    , shadow.blur
                    , shadow.red
                    , shadow.green
                    , shadow.blue
                    , shadow.alpha
                    ]

            Property _ m1 ->
                motionDone m1

            Property2 _ m1 m2 ->
                motionDone m1 && motionDone m2

            Property3 _ m1 m2 m3 ->
                List.all motionDone [ m1, m2, m3 ]

            Property4 _ m1 m2 m3 m4 ->
                List.all motionDone [ m1, m2, m3, m4 ]

            AngleProperty _ m1 ->
                motionDone m1

            Points ms ->
                List.all (\( x, y ) -> motionDone x && motionDone y) ms

            Path cmds ->
                List.all isCmdDone cmds


isCmdDone : PathCommand -> Bool
isCmdDone cmd =
    let
        motionDone motion =
            motion.velocity == 0 && motion.position == motion.target
    in
        case cmd of
            Move m1 m2 ->
                motionDone m1 && motionDone m2

            MoveTo m1 m2 ->
                motionDone m1 && motionDone m2

            Line m1 m2 ->
                motionDone m1 && motionDone m2

            LineTo m1 m2 ->
                motionDone m1 && motionDone m2

            Horizontal motion ->
                motionDone motion

            HorizontalTo motion ->
                motionDone motion

            Vertical motion ->
                motionDone motion

            VerticalTo motion ->
                motionDone motion

            Curve { control1, control2, point } ->
                (motionDone <| Tuple.first control1)
                    && (motionDone <| Tuple.second control1)
                    && (motionDone <| Tuple.first control2)
                    && (motionDone <| Tuple.second control2)
                    && (motionDone <| Tuple.first point)
                    && (motionDone <| Tuple.second point)

            CurveTo { control1, control2, point } ->
                (motionDone <| Tuple.first control1)
                    && (motionDone <| Tuple.second control1)
                    && (motionDone <| Tuple.first control2)
                    && (motionDone <| Tuple.second control2)
                    && (motionDone <| Tuple.first point)
                    && (motionDone <| Tuple.second point)

            Quadratic { control, point } ->
                (motionDone <| Tuple.first control)
                    && (motionDone <| Tuple.second control)
                    && (motionDone <| Tuple.first point)
                    && (motionDone <| Tuple.second point)

            QuadraticTo { control, point } ->
                (motionDone <| Tuple.first control)
                    && (motionDone <| Tuple.second control)
                    && (motionDone <| Tuple.first point)
                    && (motionDone <| Tuple.second point)

            SmoothQuadratic coords ->
                List.all (\( x, y ) -> motionDone x && motionDone y) coords

            SmoothQuadraticTo coords ->
                List.all (\( x, y ) -> motionDone x && motionDone y) coords

            Smooth coords ->
                List.all (\( x, y ) -> motionDone x && motionDone y) coords

            SmoothTo coords ->
                List.all (\( x, y ) -> motionDone x && motionDone y) coords

            ClockwiseArc arc ->
                motionDone arc.x
                    && motionDone arc.y
                    && motionDone arc.radius
                    && motionDone arc.startAngle
                    && motionDone arc.endAngle

            AntiClockwiseArc arc ->
                motionDone arc.x
                    && motionDone arc.y
                    && motionDone arc.radius
                    && motionDone arc.startAngle
                    && motionDone arc.endAngle

            Close ->
                True


{-| Set a new target for a style.

If a property doesn't exist in the current style, issue a warning and do nothing with that property.

If a property doesn't exist as a target, then leave it as is.

Order matters (mostly for transformation stacking)

-}
startTowards : Bool -> List Property -> List Property -> List Property
startTowards overrideInterpolation current target =
    List.filterMap
        (\propPair ->
            case propPair of
                ( cur, Just to ) ->
                    Just <| setTarget overrideInterpolation cur to

                ( prop, Nothing ) ->
                    Just prop
        )
        (zipPropertiesGreedy current target)


setTarget : Bool -> Property -> Property -> Property
setTarget overrideInterpolation current newTarget =
    let
        setMotionTarget motion targetMotion =
            let
                newMotion =
                    if overrideInterpolation then
                        { motion
                            | interpolationOverride = Just targetMotion.interpolation
                        }
                    else
                        motion
            in
                case newMotion.interpolationOverride of
                    Nothing ->
                        case newMotion.interpolation of
                            Easing ease ->
                                { newMotion
                                    | target = targetMotion.position
                                    , interpolation =
                                        Easing
                                            { ease
                                                | start = motion.position
                                                , progress = 0
                                            }
                                }

                            _ ->
                                { newMotion
                                    | target = targetMotion.position
                                }

                    Just override ->
                        case override of
                            Easing ease ->
                                { newMotion
                                    | target = targetMotion.position
                                    , interpolationOverride =
                                        Just <|
                                            Easing
                                                { ease
                                                    | start = motion.position
                                                    , progress = 0
                                                }
                                }

                            _ ->
                                { newMotion
                                    | target = targetMotion.position
                                }
    in
        case current of
            ExactProperty name value ->
                ExactProperty name value

            ColorProperty name m1 m2 m3 m4 ->
                case newTarget of
                    ColorProperty _ t1 t2 t3 t4 ->
                        ColorProperty name
                            (setMotionTarget m1 t1)
                            (setMotionTarget m2 t2)
                            (setMotionTarget m3 t3)
                            (setMotionTarget m4 t4)

                    _ ->
                        current

            ShadowProperty name inset shadow ->
                case newTarget of
                    ShadowProperty _ _ targetShadow ->
                        ShadowProperty
                            name
                            inset
                            { offsetX = setMotionTarget shadow.offsetX targetShadow.offsetX
                            , offsetY = setMotionTarget shadow.offsetY targetShadow.offsetY
                            , size = setMotionTarget shadow.size targetShadow.size
                            , blur = setMotionTarget shadow.blur targetShadow.blur
                            , red = setMotionTarget shadow.red targetShadow.red
                            , green = setMotionTarget shadow.green targetShadow.green
                            , blue = setMotionTarget shadow.blue targetShadow.blue
                            , alpha = setMotionTarget shadow.alpha targetShadow.alpha
                            }

                    _ ->
                        current

            Property name m1 ->
                case newTarget of
                    Property _ t1 ->
                        Property name
                            (setMotionTarget m1 t1)

                    _ ->
                        current

            Property2 name m1 m2 ->
                case newTarget of
                    Property2 _ t1 t2 ->
                        Property2 name
                            (setMotionTarget m1 t1)
                            (setMotionTarget m2 t2)

                    _ ->
                        current

            Property3 name m1 m2 m3 ->
                case newTarget of
                    Property3 _ t1 t2 t3 ->
                        Property3 name
                            (setMotionTarget m1 t1)
                            (setMotionTarget m2 t2)
                            (setMotionTarget m3 t3)

                    _ ->
                        current

            Property4 name m1 m2 m3 m4 ->
                case newTarget of
                    Property4 _ t1 t2 t3 t4 ->
                        Property4 name
                            (setMotionTarget m1 t1)
                            (setMotionTarget m2 t2)
                            (setMotionTarget m3 t3)
                            (setMotionTarget m4 t4)

                    _ ->
                        current

            AngleProperty name m1 ->
                case newTarget of
                    AngleProperty _ t1 ->
                        AngleProperty name
                            (setMotionTarget m1 t1)

                    _ ->
                        current

            Points currentPts ->
                case newTarget of
                    Points targetPts ->
                        let
                            ( m1s, m2s ) =
                                matchPoints currentPts targetPts
                        in
                            Points <|
                                List.map2
                                    (\( x1, y1 ) ( x2, y2 ) ->
                                        ( (setMotionTarget x1 x2)
                                        , (setMotionTarget y1 y2)
                                        )
                                    )
                                    m1s
                                    m2s

                    _ ->
                        current

            Path cmds ->
                case newTarget of
                    Path targets ->
                        Path <|
                            List.map2
                                setPathTarget
                                cmds
                                targets

                    _ ->
                        current


setPathTarget : PathCommand -> PathCommand -> PathCommand
setPathTarget cmd targetCmd =
    let
        setMotionTarget motion targetMotion =
            case motion.interpolation of
                Easing ease ->
                    { motion
                        | target = targetMotion.position
                        , interpolation =
                            Easing
                                { ease | start = motion.position }
                    }

                _ ->
                    { motion | target = targetMotion.position }
    in
        case cmd of
            Move m1 m2 ->
                case targetCmd of
                    Move t1 t2 ->
                        Move
                            (setMotionTarget m1 t1)
                            (setMotionTarget m2 t2)

                    _ ->
                        cmd

            MoveTo m1 m2 ->
                case targetCmd of
                    MoveTo t1 t2 ->
                        MoveTo
                            (setMotionTarget m1 t1)
                            (setMotionTarget m2 t2)

                    _ ->
                        cmd

            Line m1 m2 ->
                case targetCmd of
                    Line t1 t2 ->
                        Line
                            (setMotionTarget m1 t1)
                            (setMotionTarget m2 t2)

                    _ ->
                        cmd

            LineTo m1 m2 ->
                case targetCmd of
                    LineTo t1 t2 ->
                        LineTo
                            (setMotionTarget m1 t1)
                            (setMotionTarget m2 t2)

                    _ ->
                        cmd

            Horizontal m1 ->
                case targetCmd of
                    Horizontal t1 ->
                        Horizontal
                            (setMotionTarget m1 t1)

                    _ ->
                        cmd

            HorizontalTo m1 ->
                case targetCmd of
                    HorizontalTo t1 ->
                        HorizontalTo
                            (setMotionTarget m1 t1)

                    _ ->
                        cmd

            Vertical m1 ->
                case targetCmd of
                    Vertical t1 ->
                        Vertical
                            (setMotionTarget m1 t1)

                    _ ->
                        cmd

            VerticalTo m1 ->
                case targetCmd of
                    VerticalTo t1 ->
                        VerticalTo
                            (setMotionTarget m1 t1)

                    _ ->
                        cmd

            Curve points ->
                case targetCmd of
                    Curve targets ->
                        Curve
                            { control1 =
                                ( setMotionTarget (Tuple.first points.control1) (Tuple.first targets.control1)
                                , setMotionTarget (Tuple.second points.control1) (Tuple.second targets.control1)
                                )
                            , control2 =
                                ( setMotionTarget (Tuple.first points.control2) (Tuple.first targets.control2)
                                , setMotionTarget (Tuple.second points.control2) (Tuple.second targets.control2)
                                )
                            , point =
                                ( setMotionTarget (Tuple.first points.point) (Tuple.first targets.point)
                                , setMotionTarget (Tuple.second points.point) (Tuple.second targets.point)
                                )
                            }

                    _ ->
                        cmd

            CurveTo points ->
                case targetCmd of
                    CurveTo targets ->
                        CurveTo
                            { control1 =
                                ( setMotionTarget (Tuple.first points.control1) (Tuple.first targets.control1)
                                , setMotionTarget (Tuple.second points.control1) (Tuple.second targets.control1)
                                )
                            , control2 =
                                ( setMotionTarget (Tuple.first points.control2) (Tuple.first targets.control2)
                                , setMotionTarget (Tuple.second points.control2) (Tuple.second targets.control2)
                                )
                            , point =
                                ( setMotionTarget (Tuple.first points.point) (Tuple.first targets.point)
                                , setMotionTarget (Tuple.second points.point) (Tuple.second targets.point)
                                )
                            }

                    _ ->
                        cmd

            Quadratic points ->
                case targetCmd of
                    Quadratic targets ->
                        Quadratic
                            { control =
                                ( setMotionTarget (Tuple.first points.control) (Tuple.first targets.control)
                                , setMotionTarget (Tuple.second points.control) (Tuple.second targets.control)
                                )
                            , point =
                                ( setMotionTarget (Tuple.first points.point) (Tuple.first targets.point)
                                , setMotionTarget (Tuple.second points.point) (Tuple.second targets.point)
                                )
                            }

                    _ ->
                        cmd

            QuadraticTo points ->
                case targetCmd of
                    QuadraticTo targets ->
                        QuadraticTo
                            { control =
                                ( setMotionTarget (Tuple.first points.control) (Tuple.first targets.control)
                                , setMotionTarget (Tuple.second points.control) (Tuple.second targets.control)
                                )
                            , point =
                                ( setMotionTarget (Tuple.first points.point) (Tuple.first targets.point)
                                , setMotionTarget (Tuple.second points.point) (Tuple.second targets.point)
                                )
                            }

                    _ ->
                        cmd

            SmoothQuadratic coords ->
                case targetCmd of
                    SmoothQuadratic targetCoords ->
                        SmoothQuadratic <|
                            List.map2
                                (\( x1, y1 ) ( x2, y2 ) ->
                                    ( (setMotionTarget x1 x2)
                                    , (setMotionTarget y1 y2)
                                    )
                                )
                                coords
                                targetCoords

                    _ ->
                        cmd

            SmoothQuadraticTo coords ->
                case targetCmd of
                    SmoothQuadraticTo targetCoords ->
                        SmoothQuadraticTo <|
                            List.map2
                                (\( x1, y1 ) ( x2, y2 ) ->
                                    ( (setMotionTarget x1 x2)
                                    , (setMotionTarget y1 y2)
                                    )
                                )
                                coords
                                targetCoords

                    _ ->
                        cmd

            Smooth coords ->
                case targetCmd of
                    Smooth targetCoords ->
                        Smooth <|
                            List.map2
                                (\( x1, y1 ) ( x2, y2 ) ->
                                    ( (setMotionTarget x1 x2)
                                    , (setMotionTarget y1 y2)
                                    )
                                )
                                coords
                                targetCoords

                    _ ->
                        cmd

            SmoothTo coords ->
                case targetCmd of
                    SmoothTo targetCoords ->
                        SmoothTo <|
                            List.map2
                                (\( x1, y1 ) ( x2, y2 ) ->
                                    ( (setMotionTarget x1 x2)
                                    , (setMotionTarget y1 y2)
                                    )
                                )
                                coords
                                targetCoords

                    _ ->
                        cmd

            ClockwiseArc arc ->
                case targetCmd of
                    ClockwiseArc target ->
                        ClockwiseArc <|
                            let
                                x =
                                    arc.x

                                y =
                                    arc.y

                                radius =
                                    arc.radius

                                startAngle =
                                    arc.startAngle

                                endAngle =
                                    arc.endAngle
                            in
                                { arc
                                    | x = setMotionTarget x target.x
                                    , y = setMotionTarget y target.y
                                    , radius = setMotionTarget radius target.radius
                                    , startAngle = setMotionTarget startAngle target.startAngle
                                    , endAngle = setMotionTarget endAngle target.endAngle
                                }

                    _ ->
                        cmd

            AntiClockwiseArc arc ->
                case targetCmd of
                    AntiClockwiseArc target ->
                        AntiClockwiseArc <|
                            let
                                x =
                                    arc.x

                                y =
                                    arc.y

                                radius =
                                    arc.radius

                                startAngle =
                                    arc.startAngle

                                endAngle =
                                    arc.endAngle
                            in
                                { arc
                                    | x = setMotionTarget x target.x
                                    , y = setMotionTarget y target.y
                                    , radius = setMotionTarget radius target.radius
                                    , startAngle = setMotionTarget startAngle target.startAngle
                                    , endAngle = setMotionTarget endAngle target.endAngle
                                }

                    _ ->
                        cmd

            Close ->
                Close


{-| We match two sets of properties.

If a property is trying to be animated but has no initial value, a warning is logged.

Order from the original list is preserved.

-}
zipPropertiesGreedy : List Property -> List Property -> List ( Property, Maybe Property )
zipPropertiesGreedy initialProps newTargetProps =
    let
        propertyMatch prop1 prop2 =
            propertyName prop1 == propertyName prop2

        ( _, warnings, props ) =
            List.foldl
                (\_ ( stackA, stackB, result ) ->
                    case List.head stackA of
                        Nothing ->
                            ( stackA, stackB, result )

                        Just a ->
                            let
                                ( matchingBs, nonMatchingBs ) =
                                    List.partition (propertyMatch a) stackB
                            in
                                ( List.drop 1 stackA
                                , case matchingBs of
                                    [] ->
                                        nonMatchingBs

                                    b :: remainingBs ->
                                        remainingBs ++ nonMatchingBs
                                , ( a, List.head matchingBs ) :: result
                                  -- This is in reverse to avoid creating a new list each iteration
                                  -- We instead have to do a reverse later.
                                )
                )
                ( initialProps, newTargetProps, [] )
                (List.repeat (List.length initialProps) 0)

        _ =
            List.map
                (\b ->
                    Debug.log "elm-style-animation" <|
                        (propertyName b ++ " has no initial value and therefore will not be animated.")
                )
                warnings
    in
        List.reverse props


{-| Move one step in our interpolation strategy.
-}
step : Time -> List Property -> List Property
step dt props =
    let
        stepProp property =
            case property of
                ExactProperty name value ->
                    ExactProperty name value

                Property name motion ->
                    Property name (stepInterpolation dt motion)

                Property2 name motion1 motion2 ->
                    Property2 name
                        (stepInterpolation dt motion1)
                        (stepInterpolation dt motion2)

                Property3 name motion1 motion2 motion3 ->
                    Property3 name
                        (stepInterpolation dt motion1)
                        (stepInterpolation dt motion2)
                        (stepInterpolation dt motion3)

                Property4 name motion1 motion2 motion3 motion4 ->
                    Property4 name
                        (stepInterpolation dt motion1)
                        (stepInterpolation dt motion2)
                        (stepInterpolation dt motion3)
                        (stepInterpolation dt motion4)

                AngleProperty name motion ->
                    AngleProperty name (stepInterpolation dt motion)

                ColorProperty name red green blue alpha ->
                    ColorProperty name
                        (stepInterpolation dt red)
                        (stepInterpolation dt green)
                        (stepInterpolation dt blue)
                        (stepInterpolation dt alpha)

                ShadowProperty name inset shadow ->
                    ShadowProperty
                        name
                        inset
                        { offsetX = stepInterpolation dt shadow.offsetX
                        , offsetY = stepInterpolation dt shadow.offsetY
                        , size = stepInterpolation dt shadow.size
                        , blur = stepInterpolation dt shadow.blur
                        , red = stepInterpolation dt shadow.red
                        , green = stepInterpolation dt shadow.green
                        , blue = stepInterpolation dt shadow.blue
                        , alpha = stepInterpolation dt shadow.alpha
                        }

                Points points ->
                    Points <|
                        List.map
                            (\( x, y ) ->
                                ( stepInterpolation dt x
                                , stepInterpolation dt y
                                )
                            )
                            points

                Path cmds ->
                    Path <|
                        List.map (stepPath dt) cmds
    in
        List.map stepProp props


stepPath : Time -> PathCommand -> PathCommand
stepPath dt cmd =
    let
        stepCoords coords =
            List.map
                (\( x, y ) ->
                    ( stepInterpolation dt x
                    , stepInterpolation dt y
                    )
                )
                coords
    in
        case cmd of
            Move m1 m2 ->
                Move
                    (stepInterpolation dt m1)
                    (stepInterpolation dt m2)

            MoveTo m1 m2 ->
                MoveTo
                    (stepInterpolation dt m1)
                    (stepInterpolation dt m2)

            Line m1 m2 ->
                Line
                    (stepInterpolation dt m1)
                    (stepInterpolation dt m2)

            LineTo m1 m2 ->
                LineTo
                    (stepInterpolation dt m1)
                    (stepInterpolation dt m2)

            Horizontal motion ->
                Horizontal
                    (stepInterpolation dt motion)

            HorizontalTo motion ->
                HorizontalTo
                    (stepInterpolation dt motion)

            Vertical motion ->
                Vertical
                    (stepInterpolation dt motion)

            VerticalTo motion ->
                VerticalTo
                    (stepInterpolation dt motion)

            Curve { control1, control2, point } ->
                Curve
                    { control1 =
                        ( stepInterpolation dt (Tuple.first control1)
                        , stepInterpolation dt (Tuple.second control1)
                        )
                    , control2 =
                        ( stepInterpolation dt (Tuple.first control2)
                        , stepInterpolation dt (Tuple.second control2)
                        )
                    , point =
                        ( stepInterpolation dt (Tuple.first point)
                        , stepInterpolation dt (Tuple.second point)
                        )
                    }

            CurveTo { control1, control2, point } ->
                CurveTo
                    { control1 =
                        ( stepInterpolation dt (Tuple.first control1)
                        , stepInterpolation dt (Tuple.second control1)
                        )
                    , control2 =
                        ( stepInterpolation dt (Tuple.first control2)
                        , stepInterpolation dt (Tuple.second control2)
                        )
                    , point =
                        ( stepInterpolation dt (Tuple.first point)
                        , stepInterpolation dt (Tuple.second point)
                        )
                    }

            Quadratic { control, point } ->
                Quadratic
                    { control =
                        ( stepInterpolation dt (Tuple.first control)
                        , stepInterpolation dt (Tuple.second control)
                        )
                    , point =
                        ( stepInterpolation dt (Tuple.first point)
                        , stepInterpolation dt (Tuple.second point)
                        )
                    }

            QuadraticTo { control, point } ->
                QuadraticTo
                    { control =
                        ( stepInterpolation dt (Tuple.first control)
                        , stepInterpolation dt (Tuple.second control)
                        )
                    , point =
                        ( stepInterpolation dt (Tuple.first point)
                        , stepInterpolation dt (Tuple.second point)
                        )
                    }

            SmoothQuadratic coords ->
                SmoothQuadratic <| stepCoords coords

            SmoothQuadraticTo coords ->
                SmoothQuadraticTo <| stepCoords coords

            Smooth coords ->
                Smooth <| stepCoords coords

            SmoothTo coords ->
                SmoothTo <| stepCoords coords

            ClockwiseArc arc ->
                ClockwiseArc <|
                    { arc
                        | x = stepInterpolation dt arc.x
                        , y = stepInterpolation dt arc.y
                        , radius = stepInterpolation dt arc.radius
                        , startAngle = stepInterpolation dt arc.startAngle
                        , endAngle = stepInterpolation dt arc.endAngle
                    }

            AntiClockwiseArc arc ->
                AntiClockwiseArc <|
                    { arc
                        | x = stepInterpolation dt arc.x
                        , y = stepInterpolation dt arc.y
                        , radius = stepInterpolation dt arc.radius
                        , startAngle = stepInterpolation dt arc.startAngle
                        , endAngle = stepInterpolation dt arc.endAngle
                    }

            Close ->
                Close


{-|
-}
tolerance : Float
tolerance =
    0.01


{-|
-}
vTolerance : Float
vTolerance =
    0.1


{-|
-}
stepInterpolation : Time -> Motion -> Motion
stepInterpolation dtms motion =
    let
        interpolationToUse =
            Maybe.withDefault
                motion.interpolation
                motion.interpolationOverride
    in
        case interpolationToUse of
            AtSpeed { perSecond } ->
                let
                    ( newPos, finished ) =
                        if motion.position < motion.target then
                            let
                                new =
                                    motion.position + (perSecond * (dtms / 1000))
                            in
                                ( new
                                , new >= motion.target
                                )
                        else
                            let
                                new =
                                    motion.position - (perSecond * (dtms / 1000))
                            in
                                ( new
                                , new <= motion.target
                                )
                in
                    if finished then
                        { motion
                            | position = motion.target
                            , velocity = 0.0
                        }
                    else
                        { motion
                            | position = newPos
                            , velocity = perSecond * 1000
                        }

            Spring { stiffness, damping } ->
                let
                    dt =
                        dtms / 1000

                    fspring =
                        stiffness * (motion.target - motion.position)

                    fdamper =
                        (-1 * damping) * motion.velocity

                    a =
                        fspring + fdamper

                    newVelocity =
                        motion.velocity + (a * dt)

                    newPos =
                        motion.position + (newVelocity * dt)

                    dx =
                        abs (motion.target - newPos)
                in
                    if dx < tolerance && abs newVelocity < vTolerance then
                        { motion
                            | position = motion.target
                            , velocity = 0.0
                        }
                    else
                        { motion
                            | position = newPos
                            , velocity = newVelocity
                        }

            Easing { progress, duration, ease, start } ->
                let
                    newProgress =
                        if (dtms / duration) + progress < 1 then
                            (dtms / duration) + progress
                        else
                            1

                    eased =
                        ease newProgress

                    distance =
                        motion.target
                            - start

                    newPos =
                        (toFloat (truncate (((eased * distance) + start) * 10000))) / 10000

                    newVelocity =
                        if newProgress == 1 then
                            0
                        else
                            (newPos - motion.position) / dtms
                in
                    case motion.interpolationOverride of
                        Nothing ->
                            { motion
                                | position = newPos
                                , velocity = newVelocity
                                , interpolation =
                                    Easing
                                        { progress = newProgress
                                        , duration = duration
                                        , ease = ease
                                        , start = start
                                        }
                            }

                        Just override ->
                            { motion
                                | position = newPos
                                , velocity = newVelocity
                                , interpolationOverride =
                                    Just <|
                                        Easing
                                            { progress = newProgress
                                            , duration = duration
                                            , ease = ease
                                            , start = start
                                            }
                            }


{-| Ensure that two lists of points have the same number
of points by duplicating the last point of the smaller list.

-}
matchPoints : List ( Motion, Motion ) -> List ( Motion, Motion ) -> ( List ( Motion, Motion ), List ( Motion, Motion ) )
matchPoints points1 points2 =
    let
        diff =
            List.length points1 - List.length points2
    in
        if diff > 0 then
            case List.head <| List.reverse points2 of
                Nothing ->
                    ( points1, points2 )

                Just last2 ->
                    ( points1
                    , points2 ++ (List.repeat (abs diff) last2)
                    )
        else if diff < 0 then
            case List.head <| List.reverse points1 of
                Nothing ->
                    ( points1, points2 )

                Just last1 ->
                    ( points1 ++ (List.repeat (abs diff) last1)
                    , points2
                    )
        else
            ( points1, points2 )
