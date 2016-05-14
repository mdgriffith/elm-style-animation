module Style.Core exposing (Model, Action(..), Keyframe, Interruption, Style, Physics, Static, update, bake, emptyEasing)
--where

import Time exposing (Time, second)
import Style.Properties exposing (..)
import Style.Spring as Spring


type alias Model =
    { start : Maybe Time
    , elapsed : Time
    , anim : List Keyframe
    , previous : Style
    , interruption : List Interruption
    }


type alias Interruption =
    { at : Time
    , anim : List Keyframe
    }


{-| -}
type Action
    = Queue (List Keyframe)
    | Interrupt (List Keyframe)
    | Tick Time


{-| Represent a style animation.
This is a list of StylePropertys, but instead of having a static value like '5',
it has a function that takes the previous value, the current time, and provides the current value.
-}
type alias Keyframe =
    { properties : List DynamicStyleProperty
    , delay : Time
    }


type alias DynamicStyleProperty = 
        Targeted (StyleProperty Physics) (StyleProperty Static)

type alias Targeted current target
    = { current : current
      , target : target
      }

{-| Represent a CSS style as a list of style properties with concrete values.
-}
type alias Style =
    List (StyleProperty Static)

type alias Static =
    Float


type alias Physics =
    { physical : Spring.Physical
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
                anims =
                    List.drop 1 model.anim

                previous =
                    bake current model.previous

                -- if an animation finishes, but there is still an interruption pending
                -- Revise the expected interruption time down
                interruption =
                    List.map
                        (\inter ->
                            { inter | at = inter.at - totalElapsed }
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
                , anim = mapTo 0 (step elapsed dt model.previous) model.anim
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


initializeFrame : Style -> List Keyframe -> List Keyframe
initializeFrame style anims =
    let
        warn =
            case List.head anims of
                Nothing ->
                    []

                Just frame ->
                    let
                        matched = 
                            zipWith (\a b -> baseName a.current == baseName b) frame.properties style

                    in
                        List.map 
                            (\(a, maybeB) -> 
                                case maybeB of 
                                    Nothing ->
                                        let 
                                            warn = 
                                                Debug.log "elm-html-animation"
                                                    ("There is no initial value for '"
                                                    ++ id a.current
                                                    ++ "', though it is queued to be animated.  Define an initial value for '"
                                                    ++ id a.current
                                                    ++ "'")
                                        in
                                            Just warn
                                            
                                    Just b ->
                                        if id a.current == id b then
                                            Nothing
                                        else
                                            let
                                                warn =
                                                     Debug.log "elm-html-animation"
                                                        ("Wrong units provided.  "
                                                        ++ "An initial value was given as '"
                                                        ++ id b
                                                        ++ "' versus the animation which was given as '"
                                                        ++ id a.current
                                                        ++ "'.")
                                            in
                                                Just warn
                            ) matched
    in
        mapTo 0 (\a -> step 0.0 0.0 style a ) anims


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
        List.all (\p -> propIs finished p.current) frame.properties





transferVelocity : Keyframe -> Keyframe -> Keyframe
transferVelocity old new =
    let
        matched = zipWith (\a b -> id a.current == id b.current) old.properties new.properties

        newProperties = 
            List.map 
                (\(a, maybeB) -> 
                    case maybeB of 
                        Nothing ->
                            a

                        Just b ->
                            let
                                newCurrent = map2 transferVelocityProp a.current b.current
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
                zipWith (\a b -> id a.current == id b) frame.properties style 
                 |> List.map 
                        (\(a, maybeB) -> 
                            case maybeB of 
                                Nothing ->
                                    a

                                Just b ->
                                    { a
                                      | current = map3 (applyStep time dt) a.target b a.current
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
                map (\phys -> phys.physical.position) prop.current
            )
            frame.properties
        
zipWith : (a -> b -> Bool) -> List a -> List b -> List (a, Maybe b)
zipWith fn listA listB = 
    fst <| List.foldl
                (\a (stack, bStack) ->
                    let
                        (maybeB, remainingBs) =
                            case List.head bStack of 
                                Just b ->
                                    if fn a b then
                                        (Just b, Maybe.withDefault [] <| List.tail bStack)
                                    else
                                        (Nothing, bStack)

                                Nothing ->
                                    (Nothing, bStack)
                    in 
                        (stack ++ [(a, maybeB)], remainingBs)

                )
                ([], listB)
                listA





fill : Style -> Style -> Style
fill existing new = 
        zipWith (\a b -> id a == id b) existing new 
     |> List.map (\(a, maybeB) -> Maybe.withDefault a maybeB )


