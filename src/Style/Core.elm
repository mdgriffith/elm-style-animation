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


type alias Model =
    { times : Times
    , previous : Style
    , current : List Dynamic
    , frames : List Keyframe
    , interruption : List Interruption
    , defaults : Defaults
    , nudges : List Style
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


type alias Interruption =
    { at : Time
    , frame : List Keyframe
    }


{-| -}
type Action
    = Queue (List Keyframe)
    | Interrupt (List Keyframe)
    | Nudge Style
    | Tick Time


{-| Represent a style animation.
This is a list of Propertys, but instead of having a static value like '5',
it has a function that takes the previous value, the current time, and provides the current value.
-}
type Keyframe
    = Wait Time
    | To Style
    | Set Style
    | Update (List Retarget)



-- | Repeat Float Float (List Keyframe) (List Keyframe)
-- | WithSpring
-- | Send externalMsg


type alias DynamicProperty =
    Targeted Dynamic Static


type alias Targeted current target =
    { current : current
    , target : target
    }


init : Style -> Model
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
    , defaults =
        { spring =
            { stiffness = 170
            , damping = 26
            }
        }
    }


update : Action -> Model -> Model
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
                    applyNudges <| setTimes now model
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
                            case List.head model.frames of
                                Nothing ->
                                    modelWithTime

                                Just current ->
                                    tick current modelWithTime

                    Nothing ->
                        case List.head model.frames of
                            Nothing ->
                                setStart Nothing modelWithTime

                            Just current ->
                                case model.times.start of
                                    Nothing ->
                                        tick current <| setStart (Just now) modelWithTime

                                    Just s ->
                                        tick current modelWithTime


applyNudges : Model -> Model
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


setTimes : Time -> Model -> Model
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


setStart : Maybe Time -> Model -> Model
setStart start model =
    let
        times =
            model.times

        restarted =
            { times | start = start }
    in
        { model | times = restarted }


tick : Keyframe -> Model -> Model
tick currentFrame model =
    if model.times.dt == 0 || elapsed model.times < 0 then
        -- Nothing has happened
        model
    else
        let
            ( props, done, revisedFrame ) =
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
                    , frames = revisedFrame :: List.drop 1 model.frames
                }



--     if done elapsed frame then
--     -- animation is finished, switch to new frame
--     let
--         frames =
--             List.drop 1 model.frames
--
--         previous =
--             bake (step elapsed dt model.previous frame) model.previous
--
--         -- if an animation finishes, but there is still an interruption pending
--         -- Revise the expected interruption time down
--         interruption =
--             List.map
--                 (\inter ->
--                     { inter | at = inter.at - elapsed }
--                 )
--                 model.interruption
--
--         amended =
--             case List.head frames of
--                 Nothing ->
--                     previous
--
--                 Just frame ->
--                     amend previous frame
--
--         initialized =
--             mapTo 0 (initializeFrame amended amended) frames
--
--         newModel =
--             { model
--                 | elapsed = 0.0
--                 , start = Just now
--                 , previous = amended
--                 , frames = initialized
--                 , interruption = interruption
--             }
--     in
--         if List.length newModel.frames == 0 then
--             case newModel.repeatCache of
--                 Nothing ->
--                     newModel
--
--                 Just repeat ->
--                     let
--                         newRepeat =
--                             if fst repeat == 1 then
--                                 Nothing
--                             else
--                                 Just ( fst repeat - 1, snd repeat )
--                     in
--                         update (Queue (snd repeat)) { newModel | repeatCache = newRepeat }
--         else
--             newModel
-- else
--     -- normal tick
--     { model
--         | elapsed = elapsed
--         , start = Just start
--         , frames = mapTo 0 (step elapsed dt model.previous) model.frames
--     }


interrupt : Model -> List Keyframe -> List Interruption -> Model
interrupt model interruption remaining =
    let
        previous =
            case List.head model.frames of
                Nothing ->
                    model.previous

                Just frame ->
                    Style.Collection.bake model.current model.previous

        -- amended =
        --     case List.head newFrames of
        --         Nothing ->
        --             previous
        --
        --         Just frame ->
        --             amend previous frame
        --
    in
        { model
            | frames =
                interruption
            , previous = previous
            , interruption = remaining
        }



--
-- {-| amend the style to compensate for the number of points in the Points property
-- -}
-- amend : Style -> Keyframe -> Style
-- amend style frame =
--     let
--         paired =
--             zipWith (\a b -> Style.PropertyHelpers.id a == Style.PropertyHelpers.id b.target) style frame.properties
--     in
--         List.map
--             (\( styleProps, maybeFrame ) ->
--                 case maybeFrame of
--                     Nothing ->
--                         styleProps
--
--                     Just frame ->
--                         Style.PropertyHelpers.matchPoints styleProps frame.target
--             )
--             paired
--
--
-- initializeFrame : Style -> Style -> Keyframe -> Keyframe
-- initializeFrame style prevTargetStyle frame =
--     case frame of
--         Wait time ->
--             Wait time
--
--         To targetStyle ->
--             let
--                 matched =
--                     zipWith (\a b -> (Style.PropertyHelpers.baseName a.current == Style.PropertyHelpers.baseName b)) targetStyle style
--
--                 warnings =
--                     List.map
--                         (\( a, maybeB ) ->
--                             case maybeB of
--                                 Nothing ->
--                                     let
--                                         warn =
--                                             Debug.log "elm-style-animation"
--                                                 ("There is no initial value for '"
--                                                     ++ Style.PropertyHelpers.id a.current
--                                                     ++ "', though it is queued to be animated.  Define an initial value for '"
--                                                     ++ Style.PropertyHelpers.id a.current
--                                                     ++ "'"
--                                                 )
--                                     in
--                                         Just warn
--
--                                 Just b ->
--                                     if Style.PropertyHelpers.id a.current == Style.PropertyHelpers.id b then
--                                         Nothing
--                                     else
--                                         let
--                                             warn =
--                                                 Debug.log "elm-style-animation"
--                                                     ("Wrong units provided.  "
--                                                         ++ "An initial value was given as '"
--                                                         ++ Style.PropertyHelpers.id b
--                                                         ++ "' versus the animation which was given as '"
--                                                         ++ Style.PropertyHelpers.id a.current
--                                                         ++ "'."
--                                                     )
--                                         in
--                                             Just warn
--                         )
--                         matched
--
--                 retargeted =
--                     retargetIfNecessary frame prevTargetStyle
--             in
--                 step 0.0 0.0 style (matchPoints retargeted prevTargetStyle)
--
--


isDone : List Dynamic -> Bool
isDone style =
    let
        finished prop =
            Spring.atRest prop.spring prop.physical
    in
        List.all (\prop -> Style.PropertyHelpers.is finished prop) style


{-| Advance apply a keyframe using the current times.

Return the resultant style properties and indicate if the frame is finished or not.
-}
step : Keyframe -> Model -> ( List Dynamic, Bool, Keyframe )
step frame model =
    case frame of
        Wait till ->
            if till <= model.times.current then
                ( model.current, True, frame )
            else
                ( model.current, False, frame )

        Set target ->
            let
                advanced =
                    Style.Collection.map3 (setStep model)
                        model.previous
                        target
                        model.current
            in
                ( advanced, True, frame )

        To target ->
            let
                advanced =
                    Style.Collection.map3 (applyStep model)
                        model.previous
                        target
                        model.current
            in
                ( advanced, isDone advanced, frame )

        Update retarget ->
            let
                target =
                    Style.Collection.apply retarget model.previous
            in
                step (To target) model



-- Repeat i limit current cache ->
--     if i >= limit then
--         (,True, frame)
--     else
--         let
--             revisedFrame =
--                 Repeat (i+1) limit current cache
--         in
--             (advanced, False, revisedFrame)


applyStep : Model -> Float -> Float -> Physics -> Physics
applyStep model from target physics =
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

        newSpring =
            physics.spring

        targeted =
            { newSpring
                | destination = target
            }

        finalPhysical =
            Spring.update model.times.dt targeted positioned
    in
        { physics
            | physical = finalPhysical
            , spring = targeted
        }


setStep : Model -> Float -> Float -> Physics -> Physics
setStep model from target physics =
    { physics
        | physical =
            { position = target
            , velocity = 0
            , mass = physics.physical.mass
            }
    }
