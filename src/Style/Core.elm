module Style.Core exposing (Model, Action(..), Keyframe, Interruption, update, bake, empty, emptyKeyframe)
--where

import Time exposing (Time, second)
import Style.Properties exposing (Property)
import Style.PropertyHelpers exposing (..)
import Style.Spring as Spring


type alias Model =
    { start : Maybe Time
    , elapsed : Time
    , frames : List Keyframe
    , previous : Style
    , interruption : List Interruption
    , repeatCache : Maybe (Float, List Keyframe)
    }


type alias Interruption =
    { at : Time
    , frame : List Keyframe
    }


{-| -}
type Action
    = Queue (List Keyframe)
    | Interrupt (List Keyframe)
    | Repeat Float (List Keyframe)
    | QueueRepeat Float (List Keyframe)
    | Tick Time


{-| Represent a style animation.
This is a list of Propertys, but instead of having a static value like '5',
it has a function that takes the previous value, the current time, and provides the current value.
-}
type alias Keyframe =
    { properties : List DynamicProperty
    , delay : Time
    , retarget : Maybe (Int -> Static -> Static)
    }


type alias DynamicProperty =
        Targeted Dynamic Static

type alias Targeted current target
    = { current : current
      , target : target
      }



empty : Model
empty =
    { elapsed = 0.0
    , start = Nothing
    , frames = []
    , previous = []
    , interruption = []
    , repeatCache = Nothing
    }


emptyKeyframe : Keyframe
emptyKeyframe =
    { properties = []
    , delay = 0.0
    , retarget = Nothing
    }


update : Action -> Model -> Model
update action model =
    case action of
        Queue newFrames ->
            case List.head model.frames of
                Nothing ->
                    let
                        amended =
                            case List.head newFrames of
                                Nothing -> model.previous
                                Just frame ->
                                    amend model.previous frame
                        
                        initialized = mapTo 0 (initializeFrame amended amended) newFrames

                    in
                        { model
                            | frames = initialized
                            , previous = amended
                        }

                Just a ->
                    { model | frames = model.frames ++ newFrames }

        Interrupt interrupt ->
            case List.head interrupt of
                Nothing ->
                    model

                Just first ->
                    let
                        last =
                            List.head
                                <| List.reverse model.interruption

                        interruptions =
                            case last of
                                Nothing ->
                                    [ { at = model.elapsed + first.delay
                                      , frame =
                                            List.map (\i -> { i | delay = 0 })
                                                interrupt
                                            -- remove delay because we're
                                            -- already accounting for it
                                      }
                                    ]

                                Just prev ->
                                    prev
                                        :: [ { at = (model.elapsed + first.delay) - prev.at
                                             , frame =
                                                List.map (\i -> { i | delay = 0 })
                                                    interrupt
                                                -- remove delay because we're
                                                -- already accounting for it
                                             }
                                           ]
                    in
                        { model
                            | interruption = interruptions
                        }
        Repeat i frames ->
            if i <= 0 then
                model
            else if i == 1 then
                update (Interrupt frames) model
            else
                let 
                    newModel = update (Interrupt frames) model
                in
                    { newModel | repeatCache = Just (i-1, frames) }

        QueueRepeat i frames ->
            if i <= 0 then
                model
            else 
                let 
                    newModel = update (Queue frames) model
                    repeatCache =
                        if i > 1 then
                            Just (i-1, frames)
                        else
                            Nothing
                in
                    { newModel | repeatCache = repeatCache }


        Tick now ->
            let
                ( start, elapsed, dt ) =
                    getTimes now model
            in
                case List.head model.interruption of
                    Just interruption ->
                        if elapsed >= interruption.at then
                           interrupt now
                                model
                                interruption.frame
                                (List.drop 1 model.interruption)
                               
                        else
                            case List.head model.frames of
                                Nothing ->
                                    -- There is an interruption but we havent reached it yet,
                                    -- keep going
                                    continue model elapsed start

                                Just current ->
                                    tick model current elapsed dt start now

                    Nothing ->
                        case List.head model.frames of
                            Nothing ->
                                { model
                                    | elapsed = 0.0
                                    , start = Nothing
                                    , frames = []
                                }

                            Just current ->
                                tick model current elapsed dt start now


continue : Model -> Time -> Time -> Model
continue model elapsed start =
    { model
        | elapsed = elapsed
        , start = Just start
    }


tick : Model -> Keyframe -> Time -> Time -> Time -> Time -> Model
tick model current totalElapsed dt start now =
    let
        elapsed =
            totalElapsed - current.delay
    in
        if dt == 0 || elapsed < 0 then
            -- Nothing has happened
            continue model totalElapsed start
        else if done elapsed current then
            -- animation is finished, switch to new frame
            let

                frames =
                    List.drop 1 model.frames

                previous =
                    bake (step elapsed dt model.previous current) model.previous

                -- if an animation finishes, but there is still an interruption pending
                -- Revise the expected interruption time down
                interruption =
                    List.map
                        (\inter ->
                            { inter | at = inter.at - totalElapsed }
                        )
                        model.interruption

                amended =
                    case List.head frames of
                        Nothing -> previous
                        Just frame ->
                            amend previous frame

                initialized = mapTo 0 (initializeFrame amended amended) frames

                newModel = 
                    { model
                        | elapsed = 0.0
                        , start = Just now
                        , previous = amended
                        , frames = initialized
                        , interruption = interruption
                    }
            in
                if List.length newModel.frames == 0 then
                    case newModel.repeatCache of 
                        Nothing -> newModel
                        Just repeat ->
                            let 
                                newRepeat = 
                                    if fst repeat == 1 then
                                        Nothing
                                    else
                                        Just (fst repeat - 1, snd repeat)
                            in
                                update (Queue (snd repeat)) { newModel | repeatCache = newRepeat }
                else
                    newModel
                
        else
            -- normal tick
            { model
                | elapsed = elapsed
                , start = Just start
                , frames = mapTo 0 (step elapsed dt model.previous) model.frames
            }



getTimes : Time -> Model -> ( Time, Time, Time )
getTimes now model =
    let
        prelimStart =
            case model.start of
                Nothing ->
                    now

                Just t ->
                    t

        prelimElapsed =
            now - prelimStart

        prelimDt =
            prelimElapsed - model.elapsed

        -- if dt is very large (starting at, maybe, 300ms)
        -- then it's most likely because someone left the
        -- browser mid animation and then returned.
        -- The browser 'pauses' the animation until it's viewed again.
        -- the longer the user is gone, the longer the pause.
        --  This can cause very screwy results, as you might imagine.
        -- To fix this, if there is a large dt, then
        --   * start is reset
        --   * elapsed is reset
        --   * this frame is essentially skipped.
    in
        if prelimDt > 300 then
            ( now - model.elapsed, model.elapsed, 0 )
        else
            ( prelimStart, prelimElapsed, prelimDt )


interrupt : Time -> Model -> List Keyframe -> List Interruption -> Model
interrupt now model interruption remaining =
    let
        ( previous, prevTarget, newFrames ) =
            case List.head model.frames of
                Nothing ->
                    ( model.previous
                    , model.previous
                    , interruption
                    )

                Just frame ->
                    ( bake frame model.previous
                    , getTarget frame
                    , mapTo 0
                        (\newFrame ->
                           transferVelocity frame newFrame
                        )
                        interruption
                    )

        
        amended =
            case List.head newFrames of
                Nothing -> previous
                Just frame ->
                    amend previous frame

        amendedTarget =
            case List.head newFrames of
                Nothing -> prevTarget
                Just frame ->
                    amend prevTarget frame
    in
        { model
            | frames = mapTo 0 (initializeFrame amended amendedTarget) newFrames
            , elapsed = 0.0
            , start = Nothing
            , previous = amended
            , interruption = remaining
        }


getTarget : Keyframe -> Style
getTarget frame =
        List.map (\prop -> prop.target) frame.properties



{-| amend the style to compensate for the number of points in the Points property
-}
amend : Style -> Keyframe -> Style
amend style frame =
    let
        paired =  zipWith (\a b -> Style.PropertyHelpers.id a == Style.PropertyHelpers.id b.target) style frame.properties
    in
        List.map
            (\(styleProps, maybeFrame) ->
                case maybeFrame of
                    Nothing -> styleProps
                    Just frame ->
                        Style.PropertyHelpers.matchPoints styleProps frame.target
            )
        paired


initializeFrame : Style -> Style -> Keyframe -> Keyframe
initializeFrame style prevTargetStyle frame =
    let
        matched =  zipWith (\a b -> (Style.PropertyHelpers.baseName a.current == Style.PropertyHelpers.baseName b)) frame.properties style
        warnings =
            List.map
                (\(a, maybeB) ->
                    case maybeB of
                        Nothing ->
                            let
                                warn =
                                    Debug.log "elm-style-animation"
                                        ("There is no initial value for '"
                                        ++ Style.PropertyHelpers.id a.current
                                        ++ "', though it is queued to be animated.  Define an initial value for '"
                                        ++ Style.PropertyHelpers.id a.current
                                        ++ "'")
                            in
                                Just warn

                        Just b ->
                            if Style.PropertyHelpers.id a.current == Style.PropertyHelpers.id b then
                                Nothing
                            else
                                let
                                    warn =
                                         Debug.log "elm-style-animation"
                                            ("Wrong units provided.  "
                                            ++ "An initial value was given as '"
                                            ++ Style.PropertyHelpers.id b
                                            ++ "' versus the animation which was given as '"
                                            ++ Style.PropertyHelpers.id a.current
                                            ++ "'.")
                                in
                                    Just warn
                ) matched
        retargeted = retargetIfNecessary frame prevTargetStyle
    in
        step 0.0 0.0 style (matchPoints retargeted prevTargetStyle)



retargetIfNecessary : Keyframe -> Style -> Keyframe
retargetIfNecessary frame lastTargetStyle =
    case frame.retarget of
        Nothing -> frame
        Just retarget ->
            let
                possiblePairs =  zipWith (\a b -> Style.PropertyHelpers.id a.target == Style.PropertyHelpers.id b) frame.properties lastTargetStyle
                pairs = List.filterMap 
                            (\(prop, style) ->
                                case style of 
                                    Nothing -> Nothing
                                    Just s ->
                                        Just (prop, s)

                            ) possiblePairs 
            in
                { frame |
                    properties =
                        mapWithCount 
                            (\i (prop, prevStyle) ->
                                { prop 
                                    | target = retarget i prevStyle
                                }
                            ) pairs
                }


getPropCount x list =
    List.foldl (\y acc ->
                    if Style.PropertyHelpers.id x == Style.PropertyHelpers.id y then
                        acc+1
                    else acc
                ) 1 list


mapWithCount fn list =
    let
        mapped =
             List.foldl
                (\x acc ->
                    let
                        count = getPropCount (snd x) acc.past
                    in
                        { current = acc.current ++ [fn count x]
                        , past = acc.past ++ [snd x]
                        }
                ) { current = []
                  , past = []
                  } 
                list
    in mapped.current



matchPoints : Keyframe -> Style -> Keyframe
matchPoints frame lastTargetStyle =
    let
        paired =  zipWith (\a b -> Style.PropertyHelpers.id a.target == Style.PropertyHelpers.id b) frame.properties lastTargetStyle
    in
        { frame |
            properties =
                List.map
                    (\(frameProps, maybeLastTarget) ->
                        case maybeLastTarget of
                            Nothing -> frameProps
                            Just lastTarget ->
                                { frameProps
                                    | target = Style.PropertyHelpers.matchPoints frameProps.target lastTarget
                                    , current = Style.PropertyHelpers.matchPoints frameProps.current lastTarget
                                }
                    )
                paired
        }





done : Time -> Keyframe -> Bool
done time frame =
    let
        finished prop =
            case prop.easing of
                Nothing ->
                    Spring.atRest prop.spring prop.physical

                Just easing ->
                        time >= easing.duration
                     && easing.counterForcePhys == Nothing
    in
        List.all (\p -> Style.PropertyHelpers.is finished p.current) frame.properties





transferVelocity : Keyframe -> Keyframe -> Keyframe
transferVelocity old new =
    let
        matched = zipWith (\a b -> Style.PropertyHelpers.id a.current == Style.PropertyHelpers.id b.current) old.properties new.properties

        newProperties =
            List.map
                (\(a, maybeB) ->
                    case maybeB of
                        Nothing ->
                            a

                        Just b ->
                            let
                                newCurrent = Style.PropertyHelpers.updateFrom transferVelocityProp a.current b.current
                            in
                                { b | current = newCurrent }
                ) matched
    in
        { new | properties = newProperties }



transferVelocityProp : Physics -> Physics -> Physics
transferVelocityProp old target =
            let
                newPhys =
                    target.physical

                newV =
                    { newPhys | velocity = old.physical.velocity }

                -- If the target physics is easing based,
                --  calculate a new velocity based on
                -- what the easing velocity will be minus the old.physical.velocity
                --  Everyhting left over will transfer to the counterForce spring
            in
                case target.easing of
                    Nothing ->
                        { target | physical = newV }

                    Just easing ->
                        let
                            sampleSize =
                                16.0

                            -- how many milliseconds to take the sample at
                            eased =
                                if easing.duration <= 0 then
                                    1.0
                                else
                                    easing.ease (sampleSize / easing.duration)

                            easeV =
                                velocity 0 eased sampleSize

                            -- easing initial velocity
                            deltaV =
                                old.physical.velocity - easeV

                            newEasing =
                                Just
                                    <| { easing
                                        | counterForcePhys =
                                            Just
                                                <| { position = 0
                                                   , velocity = deltaV
                                                   }
                                       }
                        in
                            { target
                                | easing = newEasing
                                , physical = newV
                            }



velocity : Float -> Float -> Time -> Float
velocity oldPos newPos dt =
    (newPos - oldPos) / dt



{-| Advance a Keyframe given the existing style and the current times.
-}
step : Time -> Time -> Style -> Keyframe -> Keyframe
step time dt style frame =
     let
        newProperties =
            zipWith (\a b -> Style.PropertyHelpers.id a.current == Style.PropertyHelpers.id b) frame.properties style
                 |> List.map
                        (\(a, maybeB) ->
                            case maybeB of
                                Nothing ->
                                    a

                                Just b ->
                                    { a
                                      | current = Style.PropertyHelpers.updateOver (applyStep time dt) a.target b a.current
                                    }
                        )
    in
        { frame | properties = newProperties }


applyStep : Time -> Time -> Float -> Float -> Physics -> Physics
applyStep current dt target from physics =
        case physics.easing of
            Nothing ->
                --physics
                let
                    positioned =
                        -- Kind of a hack to establish initial values :/
                        if current == 0.0 && dt == 0.0 then
                            { position = from
                            , velocity = physics.physical.velocity
                            }
                        else
                            physics.physical

                    newSpring =
                        physics.spring

                    targeted =
                        { newSpring
                            | destination = target
                            --| destination = physics.target from 1.0
                        }

                    --positioned =
                    --  { newPhysical
                    --    | position = pos
                    --  }
                    finalPhysical =
                        Spring.update dt targeted positioned
                in
                    { physics
                        | physical = finalPhysical
                        , spring = targeted
                    }

            Just easing ->
                let

                    eased =
                        if easing.duration <= 0 then
                            1.0
                        else if current > easing.duration then
                            1.0
                        else
                            easing.ease (current / easing.duration)

                    physical =
                        physics.physical

                    currentPos =
                        ((target - from) * eased) + from
                        --physics.target from eased

                    counterSpring =
                        case easing.counterForcePhys of
                            Nothing ->
                                Just easing

                            Just phys ->
                                let
                                    newCounterSpring =
                                        Spring.update dt easing.counterForce phys
                                in
                                    if Spring.atRest easing.counterForce newCounterSpring then
                                        Just
                                            <| { easing
                                                | counterForcePhys = Nothing
                                               }
                                    else
                                        Just
                                            <| { easing
                                                | counterForcePhys = Just newCounterSpring
                                               }

                    finalPhysical =
                        { physical
                            | position = currentPos
                            , velocity = velocity physics.physical.position currentPos dt
                        }
                in
                    { physics
                        | physical = finalPhysical
                        , easing = counterSpring
                        }


mapTo : Int -> (a -> a) -> List a -> List a
mapTo i fn xs =
    let
        update j x =
            if j == i then
                fn x
            else
                x
    in
        List.indexedMap update xs


bake : Keyframe -> Style -> Style
bake frame style =
    fill style
        <| List.map
            (\prop ->
                toStatic prop.current
            )
            frame.properties

zipWith : (a -> b -> Bool) -> List a -> List b -> List (a, Maybe b)
zipWith fn listA listB =
    fst <| List.foldl
                (\a (stack, bStack) ->
                    let
                        (matching, unmatching) = List.partition (\b -> fn a b) bStack
                        maybeB = List.head matching
                        remaining = Maybe.withDefault [] <| List.tail matching
                    in
                        (stack ++ [(a, maybeB)], unmatching ++ remaining)

                )
                ([], listB)
                listA





fill : Style -> Style -> Style
fill existing new =
        zipWith (\a b -> Style.PropertyHelpers.id a == Style.PropertyHelpers.id b) existing new
     |> List.map (\(a, maybeB) -> Maybe.withDefault a maybeB )
