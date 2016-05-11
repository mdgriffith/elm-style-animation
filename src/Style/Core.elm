module Style.Core exposing (Model, Action(..), StyleKeyframe, Interruption, Style, Physics, DynamicTarget, Static, update, step, bake, emptyEasing)
--where

import Time exposing (Time, second)
import Style.Properties exposing (..)
import Style.Spring as Spring


type alias Model =
    { start : Maybe Time
    , elapsed : Time
    , anim : List StyleKeyframe
    , previous : Style
    , interruption : List Interruption
    }


type alias Interruption =
    { at : Time
    , anim : List StyleKeyframe
    }


{-| -}
type Action
    = Queue (List StyleKeyframe)
    | Interrupt (List StyleKeyframe)
    | Tick Time


{-| Represent a style animation.
This is a list of StylePropertys, but instead of having a static value like '5',
it has a function that takes the previous value, the current time, and provides the current value.
-}
type alias StyleKeyframe =
    { target : List (StyleProperty (Physics DynamicTarget))
    , delay : Time
    }


{-| Represent a CSS style as a list of style properties with concrete values.
-}
type alias Style =
    List (StyleProperty Static)


type alias DynamicTarget =
    Float -> Float -> Float


type alias Static =
    Float


type alias Physics a =
    { target : a
    , physical : Spring.Physical
    , spring : Spring.Model
    , easing : Maybe Easing
    }


type alias Easing =
    { ease : Float -> Float
    , counterForce : Spring.Model
    , counterForcePhys : Maybe Spring.Physical
    , duration : Time
    }


emptyEasing =
    { ease = defaultEasing
    , counterForce =
        { stiffness = 170
        , damping = 26
        , destination = 1
        }
    , counterForcePhys = Nothing
    , duration = defaultDuration
    }


defaultDuration : Float
defaultDuration =
    0.35 * second


defaultEasing : Float -> Float
defaultEasing x =
    (1 - cos (pi * x)) / 2


update : Action -> Model -> Model
update action model =
    case action of
        Queue anims ->
            case List.head model.anim of
                Nothing ->
                    { model | anim = initializeFrame model.previous anims }

                Just a ->
                    { model | anim = model.anim ++ anims }

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
                                      , anim =
                                            List.map (\i -> { i | delay = 0 })
                                                interrupt
                                            -- remove delay because we're
                                            -- already accounting for it
                                      }
                                    ]

                                Just prev ->
                                    prev
                                        :: [ { at = (model.elapsed + first.delay) - prev.at
                                             , anim =
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
                                interruption.anim
                                (List.drop 1 model.interruption)
                        else
                            case List.head model.anim of
                                Nothing ->
                                    -- There is an interruption but we havent reached it yet,
                                    -- keep going
                                    continue model elapsed start

                                Just current ->
                                    tick model current elapsed dt start now

                    Nothing ->
                        case List.head model.anim of
                            Nothing ->
                                { model
                                    | elapsed = 0.0
                                    , start = Nothing
                                    , anim = []
                                }

                            Just current ->
                                tick model current elapsed dt start now


continue : Model -> Time -> Time -> Model
continue model elapsed start =
    { model
        | elapsed = elapsed
        , start = Just start
    }


tick : Model -> StyleKeyframe -> Time -> Time -> Time -> Time -> Model
tick model current elapsed dt start now =
    let
        frameElapsed =
            elapsed - current.delay
    in
        if dt == 0 || frameElapsed < 0 then
            -- Nothing has happened
            continue model elapsed start
        else if done frameElapsed current then
            -- animation is finished, switch to new frame
            let
                anims =
                    List.drop 1 model.anim

                previous =
                    bake current model.previous

                -- if an animation finishes, but there is still an interruption pending
                -- Revise the expected interruption time down
                interruption =
                    List.map
                        (\inter ->
                            { inter | at = inter.at - elapsed }
                        )
                        model.interruption
            in
                { model
                    | elapsed = 0.0
                    , start = Just now
                    , previous = previous
                    , anim = initializeFrame previous anims
                    , interruption = interruption
                }
        else
            -- normal tick
            { model
                | elapsed = elapsed
                , start = Just start
                , anim = mapTo 0 (\a -> step a model.previous frameElapsed dt) model.anim
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


interrupt : Time -> Model -> List StyleKeyframe -> List Interruption -> Model
interrupt now model interruption remaining =
    let
        ( previous, newAnims ) =
            case List.head model.anim of
                Nothing ->
                    ( model.previous
                    , interruption
                    )

                Just frame ->
                    ( bake frame model.previous
                    , mapTo 0 (\a -> transferVelocity frame a) interruption
                    )
    in
        { model
            | anim = initializeFrame previous newAnims
            , elapsed = 0.0
            , start = Nothing
            , previous = previous
            , interruption = remaining
        }


initializeFrame : Style -> List StyleKeyframe -> List StyleKeyframe
initializeFrame style anims =
    let
        warn =
            case List.head anims of
                Nothing ->
                    []

                Just first ->
                    List.foldl
                        (\x acc ->
                            -- need to know how many times x has shown up already.
                            let
                                xI =
                                    countOccurance x acc
                            in
                                case findNearProp style x xI of
                                    Nothing ->
                                        let
                                            warn =
                                                Debug.log "elm-html-animation"
                                                    <| "There is no initial value for '"
                                                    ++ id x
                                                    ++ "', though it is queued to be animated.  Define an initial value for '"
                                                    ++ id x
                                                    ++ "'"
                                        in
                                            acc

                                    Just prevX ->
                                        if id x == id prevX then
                                            acc ++ [ x ]
                                        else
                                            let
                                                warn =
                                                    Debug.log "elm-html-animation"
                                                        <| "Wrong units provided.  "
                                                        ++ "An initial value was given as '"
                                                        ++ id prevX
                                                        ++ "' versus the animation which was given as '"
                                                        ++ id x
                                                        ++ "'."
                                            in
                                                acc
                        )
                        []
                        first.target
    in
        mapTo 0 (\a -> step a style 0.0 0.0) anims


done : Time -> StyleKeyframe -> Bool
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
        List.all (propIs finished) frame.target





transferVelocityProp : Maybe (Physics DynamicTarget) -> Physics DynamicTarget -> Physics DynamicTarget
transferVelocityProp maybeOld target =
    case maybeOld of
        Nothing ->
            target

        Just old ->
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


transferVelocity : StyleKeyframe -> StyleKeyframe -> StyleKeyframe
transferVelocity old new =
    let
        style =
            List.foldl
                (\x acc ->
                    -- need to know how many times x has shown up already.
                    let
                        xI =
                            countOccurance x acc
                    in
                        case findProp old.target x xI of
                            Nothing ->
                                let
                                    warn =
                                        Debug.log "elm-html-animation"
                                            """You're trying to animate """
                                            ++ id x
                                            ++ ", but haven't provided an init value for it.  It won't be animated until you do."
                                in
                                    acc

                            Just prevX ->
                                acc ++ [ stepProp x prevX transferVelocityProp ]
                )
                []
                new.target
    in
        { new | target = style }


applyStep : Time -> Time -> Maybe Float -> Physics DynamicTarget -> Physics DynamicTarget
applyStep current dt maybeFrom physics =
    case maybeFrom of
        Nothing ->
            physics

        Just from ->
            case physics.easing of
                Nothing ->
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
                                | destination = physics.target from 1.0
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
                            else
                                easing.ease (current / easing.duration)

                        physical =
                            physics.physical

                        currentPos =
                            physics.target from eased

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


velocity : Float -> Float -> Time -> Float
velocity oldPos newPos dt =
    (newPos - oldPos) / dt


step : StyleKeyframe -> Style -> Time -> Time -> StyleKeyframe
step frame prev time dt =
    let
        style =
            List.foldl
                (\x acc ->
                    -- need to know how many times x has shown up already.
                    let
                        xI =
                            countOccurance x acc
                    in
                        case findProp prev x xI of
                            Nothing ->
                                acc

                            Just prevX ->
                                acc ++ [ stepProp x prevX <| applyStep time dt ]
                )
                []
                frame.target
    in
        { frame | target = style }




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


bake : StyleKeyframe -> Style -> Style
bake frame style =
    fill
        (List.map (mapProp toStatic)
            frame.target
        )
        style


toStatic : Physics DynamicTarget -> Static
toStatic physic =
    physic.physical.position



{-|
 propCount refers to the how many times a property shows up
 in the original list that prop is being pulled from
-}
findProp : List (StyleProperty a) -> StyleProperty b -> Int -> Maybe (StyleProperty a)
findProp state prop propCount =
    let
        findBy fn xs =
            List.head
                <| List.drop propCount
                <| List.filter fn
                <| xs

        matchPropID a b =
            id a == id b
    in
        findBy (matchPropID prop) state


findNearProp : List (StyleProperty a) -> StyleProperty b -> Int -> Maybe (StyleProperty a)
findNearProp state prop propCount =
    let
        findBy fn xs =
            List.head
                <| List.drop propCount
                <| List.filter fn
                <| xs

        matchPropID a b =
            baseName a == baseName b
    in
        findBy (matchPropID prop) state


countOccurance x pool =
    List.foldl
        (\x2 count ->
            if id x == id x2 then
                count + 1
            else
                count
        )
        0
        pool


fill : List (StyleProperty Static) -> List (StyleProperty Static) -> List (StyleProperty Static)
fill new existing =
    List.foldl
        (\x acc ->
            -- need to know the id of x, meaning how many times it's shown up already.
            let
                xI =
                    countOccurance x acc
            in
                case findProp new x xI of
                    Nothing ->
                        acc ++ [ x ]

                    Just newX ->
                        acc ++ [ newX ]
        )
        []
        existing
