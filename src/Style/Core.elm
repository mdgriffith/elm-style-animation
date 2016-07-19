module Style.Core
    exposing
        ( Model
        , Action(..)
        , Keyframe(..)
        , Interruption
        , update
        , init
        )

import Time exposing (Time, second)
import Style.Properties exposing (Property)
import Style.PropertyHelpers exposing (..)
import Style.Collection
import Style.Spring as Spring


type alias Model msg =
    { times : Times
    , previous : Style
    , current : List Dynamic
    , frames : List (Keyframe msg)
    , interruption : List (Interruption msg)
    , defaults : Defaults
    , nudges : List Style
    , messages : List msg
    }


type alias Defaults =
    { spring : Spring.Preset }


type alias Times =
    { current : Time
    , start : Maybe Time
    , dt : Time
    }


elapsed : Times -> Time
elapsed times =
    case times.start of
        Nothing ->
            0

        Just t ->
            times.current - t


type alias Interruption msg =
    { at : Time
    , frame : List (Keyframe msg)
    }


{-| -}
type Action msg
    = Queue (List (Keyframe msg))
    | Interrupt (List (Keyframe msg))
    | Nudge Style
    | Tick Time


{-| Represent a style animation.
This is a list of Propertys, but instead of having a static value like '5',
it has a function that takes the previous value, the current time, and provides the current value.
-}
type Keyframe msg
    = Wait Time
    | WaitTill Time
    | To Style
    | WithSpringTo Spring.Preset Style
    | Set Style
    | Update (List Retarget)
    | Send msg



-- | Repeat Float Float (List Keyframe) (List Keyframe)
-- | WithSpring
-- | Send externalMsg


type alias DynamicProperty =
    Targeted Dynamic Static


type alias Targeted current target =
    { current : current
    , target : target
    }


init : Style -> Model msg
init static =
    { times =
        { current = 0.0
        , start = Nothing
        , dt = 0.0
        }
    , frames = []
    , current =
        List.map
            (Style.PropertyHelpers.toDynamic
                { stiffness = 170
                , damping = 26
                }
            )
            static
    , previous = static
    , interruption = []
    , nudges = []
    , messages = []
    , defaults =
        { spring =
            { stiffness = 170
            , damping = 26
            }
        }
    }


update : Action msg -> Model msg -> Model msg
update action model =
    case action of
        Queue frames ->
            { model | frames = model.frames ++ frames }

        Nudge nudge ->
            { model
                | nudges = nudge :: model.nudges
            }

        Interrupt frames ->
            case List.head frames of
                Nothing ->
                    model

                Just (Wait time) ->
                    if List.length frames < 2 then
                        model
                    else
                        { model
                            | interruption =
                                { at = model.times.current + time
                                , frame = List.drop 1 frames
                                }
                                    :: model.interruption
                        }

                _ ->
                    { model
                        | interruption =
                            { at = model.times.current
                            , frame = frames
                            }
                                :: model.interruption
                    }

        Tick now ->
            let
                modelWithTime =
                    getMessages <| initWaitTimes <| applyNudges <| setTimes now model
            in
                case List.head model.interruption of
                    Just interruption ->
                        if modelWithTime.times.current >= interruption.at then
                            setStart (Just now) <|
                                interrupt
                                    model
                                    interruption.frame
                                    (List.drop 1 model.interruption)
                        else
                            case List.head modelWithTime.frames of
                                Nothing ->
                                    modelWithTime

                                Just current ->
                                    tick current modelWithTime

                    Nothing ->
                        case List.head modelWithTime.frames of
                            Nothing ->
                                setStart Nothing modelWithTime

                            Just current ->
                                case modelWithTime.times.start of
                                    Nothing ->
                                        tick current <| setStart (Just now) modelWithTime

                                    Just s ->
                                        tick current modelWithTime


interrupt : Model msg -> List (Keyframe msg) -> List (Interruption msg) -> Model msg
interrupt model interruption remaining =
    { model
        | frames = interruption
        , interruption = remaining
        , previous =
            case List.head model.frames of
                Nothing ->
                    model.previous

                Just frame ->
                    Style.Collection.bake model.current model.previous
    }


getMessages : Model msg -> Model msg
getMessages model =
    let
        isSendMsg frame =
            case frame of
                Send _ ->
                    True

                _ ->
                    False

        extractMsg frame =
            case frame of
                Send x ->
                    Just x

                _ ->
                    Nothing

        msgs =
            List.filterMap extractMsg <|
                takeWhile isSendMsg model.frames

        remaining =
            dropWhile isSendMsg model.frames
    in
        { model
            | messages = msgs ++ model.messages
            , frames = remaining
        }


initWaitTimes : Model msg -> Model msg
initWaitTimes model =
    case List.head model.frames of
        Nothing ->
            model

        Just frame ->
            case frame of
                Wait rel ->
                    { model
                        | frames =
                            WaitTill (rel + model.times.current)
                                :: List.drop 1 model.frames
                    }

                _ ->
                    model


{-| Take elements in order as long as the predicate evaluates to `True`
-}
takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if (predicate x) then
                x :: takeWhile predicate xs
            else
                []


{-| Drop elements in order as long as the predicate evaluates to `True`
-}
dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if (predicate x) then
                dropWhile predicate xs
            else
                list


applyNudges : Model msg -> Model msg
applyNudges model =
    { model
        | nudges = []
        , current =
            List.foldl
                (\nudge current ->
                    Style.Collection.map2
                        (\x phys ->
                            let
                                physical =
                                    phys.physical

                                newVelocity =
                                    { physical | velocity = physical.velocity + x }
                            in
                                { phys | physical = newVelocity }
                        )
                        nudge
                        current
                )
                model.current
                model.nudges
    }


setTimes : Time -> Model msg -> Model msg
setTimes now model =
    let
        dt =
            now - model.times.current

        newTimes =
            { current = now
            , dt =
                if dt > 300 then
                    0.0
                else
                    dt
            , start = model.times.start
            }
    in
        { model | times = newTimes }


setStart : Maybe Time -> Model msg -> Model msg
setStart start model =
    let
        times =
            model.times

        restarted =
            { times | start = start }
    in
        { model | times = restarted }


tick : Keyframe msg -> Model msg -> Model msg
tick currentFrame model =
    if model.times.dt == 0 || elapsed model.times < 0 then
        -- Nothing has happened
        model
    else
        let
            ( props, done ) =
                step currentFrame model
        in
            if done then
                setStart
                    (if List.length model.frames > 1 then
                        Just model.times.current
                     else
                        Nothing
                    )
                    { model
                        | current = props
                        , frames = List.drop 1 model.frames
                        , previous = Style.Collection.bake props model.previous
                    }
            else
                { model
                    | current = props
                }


{-| Advance apply a keyframe using the current times.

Return the resultant style properties and indicate if the frame is finished or not.
-}
step : Keyframe msg -> Model msg -> ( List Dynamic, Bool )
step frame model =
    case frame of
        WaitTill till ->
            if till <= model.times.current then
                ( model.current, True )
            else
                ( model.current, False )

        Wait _ ->
            -- This action should never occur because all 'Wait' actions
            -- are transformed into WaitTill via `initWaitTimes`
            ( model.current, True )

        Send msg ->
            ( model.current, True )

        Set target ->
            let
                advanced =
                    Style.Collection.map3 (setStep model)
                        model.previous
                        target
                        model.current
            in
                ( advanced, True )

        To target ->
            let
                advanced =
                    Style.Collection.map3 (applyStep model model.defaults.spring)
                        model.previous
                        target
                        model.current
            in
                ( advanced, isDone advanced )

        WithSpringTo spring target ->
            let
                advanced =
                    Style.Collection.map3 (applyStep model spring)
                        model.previous
                        target
                        model.current
            in
                ( advanced, isDone advanced )

        Update retarget ->
            let
                target =
                    Style.Collection.apply retarget model.previous
            in
                step (To target) model


isDone : List Dynamic -> Bool
isDone style =
    let
        finished prop =
            Spring.atRest prop.spring prop.physical
    in
        List.all (\prop -> Style.PropertyHelpers.is finished prop) style


applyStep : Model msg -> Spring.Preset -> Float -> Float -> Physics -> Physics
applyStep model spring from target physics =
    let
        positioned =
            -- a hack to establish initial values :/
            if elapsed model.times == 0.0 then
                { position = from
                , velocity = physics.physical.velocity
                , mass = 1
                }
            else
                physics.physical

        targeted =
            { destination = target
            , stiffness = spring.stiffness
            , damping = spring.damping
            }

        finalPhysical =
            Spring.update model.times.dt targeted positioned
    in
        { physics
            | physical = finalPhysical
            , spring = targeted
        }


setStep : Model msg -> Float -> Float -> Physics -> Physics
setStep model from target physics =
    { physics
        | physical =
            { position = target
            , velocity = 0
            , mass = physics.physical.mass
            }
    }
