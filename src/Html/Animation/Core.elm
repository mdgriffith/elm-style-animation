module Html.Animation.Core (Model, Action(..), StyleKeyframe, Interruption, Style, Physics, DynamicTarget, Static, update, step, mapProp, bake, emptyEasing) where

import Time exposing (Time, second)
import Effects exposing (Effects)
import Html.Animation.Properties exposing (..)
import Html.Animation.Spring as Spring
import Html.Animation.Render as Render
import Debug


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


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Queue anims ->
      case List.head model.anim of
        Nothing ->
            ( { model 
                  | anim = initializeFrame model.previous anims }
            , Effects.tick Tick
            )

        Just a ->
          ( { model | anim = model.anim ++ anims }
          , Effects.tick Tick
          )

    Interrupt interrupt ->
      case List.head interrupt of
        Nothing ->
          ( model, Effects.none )

        Just first ->
          let
            last =
              List.head
                <| List.reverse model.interruption

            interruptions =
              case last of 
                Nothing ->
                  [ { at = model.elapsed + first.delay
                    , anim = List.map 
                                  (\i -> { i | delay = 0 } )
                                     interrupt
                             -- remove delay because we're 
                             -- already accounting for it
                    }
                  ]
                Just prev ->
                   prev ::
                      [ { at = (model.elapsed + first.delay) - prev.at
                        , anim = List.map 
                                    (\i -> { i | delay = 0 })
                                       interrupt
                                 -- remove delay because we're 
                                 -- already accounting for it
                        }
                      ]

          in
            ( { model
                | interruption = interruptions
              }
            , Effects.tick Tick
            )

    Tick now ->
      let
        ( start, elapsed, dt ) =
          getTimes now model
      in
        case List.head model.interruption of
          Just interruption ->
            if elapsed >= interruption.at then
              interrupt now model interruption.anim 
                      ( List.drop 1 model.interruption )
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
                ( { model
                    | elapsed = 0.0
                    , start = Nothing
                    , anim = []
                  }
                , Effects.none
                )

              Just current ->
                tick model current elapsed dt start now


continue : Model -> Time -> Time -> ( Model, Effects Action )
continue model elapsed start =
  ( { model
      | elapsed = elapsed
      , start = Just start
    }
  , Effects.tick Tick
  )


tick : Model -> StyleKeyframe -> Time -> Time -> Time -> Time -> ( Model, Effects Action )
tick model current elapsed dt start now =
  let 
    frameElapsed = elapsed - current.delay
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
        ( { model
            | elapsed = 0.0
            , start = Just now
            , previous = previous
            , anim = initializeFrame previous anims
            , interruption = interruption
          }
        , Effects.tick Tick
        )
    else
      -- normal tick
      ( { model
          | elapsed = elapsed
          , start = Just start
          , anim = mapTo 0 (\a -> step a model.previous frameElapsed dt) model.anim
        }
      , Effects.tick Tick
      )


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


interrupt : Time -> Model -> List StyleKeyframe -> List Interruption -> ( Model, Effects Action )
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
    ( { model
        | anim = initializeFrame previous newAnims
        , elapsed = 0.0
        , start = Nothing
        , previous = previous
        , interruption = remaining
      }
    , Effects.tick Tick
    )


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
                          ++ Render.id x
                          ++ "', though it is queued to be animated.  Define an initial value for '" 
                          ++ Render.id x ++ "'"
                    in
                      acc

                  Just prevX ->
                    if Render.id x == Render.id prevX then
                      acc ++ [ x ]
                    else
                      let
                        warn =
                          Debug.log "elm-html-animation"
                            <| "Wrong units provided.  "
                            ++ "An initial value was given as '"
                            ++ Render.id prevX
                            ++ "' versus the animation which was given as '"
                            ++ Render.id x
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
  List.all (propDone time) frame.target

propDone : Time -> StyleProperty (Physics DynamicTarget) -> Bool
propDone time prop =
  let
    isDone prop =
      case prop.easing of
        Nothing ->
          Spring.atRest prop.spring prop.physical

        Just easing ->
          time
            >= easing.duration
            && easing.counterForcePhys
            == Nothing
  in
    case prop of
      Prop _ a _ ->
        isDone a

      Display mode ->
        True

      Opacity a ->
        isDone a

      Height a _ ->
        isDone a

      Width a _ ->
        isDone a

      Left a _ ->
        isDone a

      Top a _ ->
        isDone a

      Right a _ ->
        isDone a

      Bottom a _ ->
        isDone a

      MaxHeight a _ ->
        isDone a

      MaxWidth a _ ->
        isDone a

      MinHeight a _ ->
        isDone a

      MinWidth a _ ->
        isDone a

      Padding a _ ->
        isDone a

      PaddingLeft a _ ->
        isDone a

      PaddingRight a _ ->
        isDone a

      PaddingTop a _ ->
        isDone a

      PaddingBottom a _ ->
        isDone a

      Margin a _ ->
        isDone a

      MarginLeft a _ ->
        isDone a

      MarginRight a _ ->
        isDone a

      MarginTop a _ ->
        isDone a

      MarginBottom a _ ->
        isDone a

      BorderWidth a _ ->
        isDone a

      BorderRadius a _ ->
        isDone a

      BorderTopLeftRadius a _ ->
        isDone a

      BorderTopRightRadius a _ ->
        isDone a

      BorderBottomLeftRadius a _ ->
        isDone a

      BorderBottomRightRadius a _ ->
        isDone a

      LetterSpacing a _ ->
        isDone a

      LineHeight a _ ->
        isDone a

      BackgroundPosition x y _ ->
        isDone x && isDone y

      TransformOrigin x y z _ ->
        isDone x && isDone y && isDone z

      Color x y z a ->
        isDone x && isDone y && isDone z && isDone a

      BackgroundColor x y z a ->
        isDone x && isDone y && isDone z && isDone a

      BorderColor x y z a ->
        isDone x && isDone y && isDone z && isDone a

      Translate a1 a2 _ ->
        isDone a1 && isDone a2

      Translate3d a1 a2 a3 _ ->
        isDone a1 && isDone a2 && isDone a3

      TranslateX a _ ->
        isDone a

      TranslateY a _ ->
        isDone a

      Scale a1 ->
        isDone a1

      Scale3d a1 a2 a3 ->
        isDone a1 && isDone a2 && isDone a3

      ScaleX a ->
        isDone a

      ScaleY a ->
        isDone a

      ScaleZ a ->
        isDone a

      Rotate a _ ->
        isDone a

      Rotate3d a1 a2 a3 a4 _ ->
        isDone a1 && isDone a2 && isDone a3 && isDone a4

      RotateX a _ ->
        isDone a

      RotateY a _ ->
        isDone a

      Skew a1 a2 _ ->
        isDone a1 && isDone a2

      SkewX a _ ->
        isDone a

      SkewY a _ ->
        isDone a

      Perspective a ->
        isDone a

      Matrix a b c x y z ->
        List.all isDone [ a, b, c, x, y, z ]

      Matrix3d a b c d e f g h i j k l m n o p ->
        List.all isDone [ a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p ]


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
                    Debug.log
                      "elm-html-animation"
                      """You're trying to animate """
                      ++ Render.id x
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


stepProp : StyleProperty a -> StyleProperty b -> (Maybe b -> a -> a) -> StyleProperty a
stepProp prop prev val =
  case prop of
    Prop name to unit ->
      let
        from =
          case prev of
            Prop _ x _ ->
              Just x

            _ ->
              Nothing
      in
        Prop name (val from to) unit

    Display mode -> 
      Display mode
     
    Opacity to ->
      let
        from =
          case prev of
            Opacity x ->
              Just x

            _ ->
              Nothing
      in
        Opacity (val from to)

    Height to unit ->
      let
        from =
          case prev of
            Height x _ ->
              Just x

            _ ->
              Nothing
      in
        Height (val from to) unit

    Width to unit ->
      let
        from =
          case prev of
            Width x _ ->
              Just x

            _ ->
              Nothing
      in
        Width (val from to) unit

    Left to unit ->
      let
        from =
          case prev of
            Left x _ ->
              Just x

            _ ->
              Nothing
      in
        Left (val from to) unit

    Top to unit ->
      let
        from =
          case prev of
            Top x _ ->
              Just x

            _ ->
              Nothing
      in
        Top (val from to) unit

    Right to unit ->
      let
        from =
          case prev of
            Right x _ ->
              Just x

            _ ->
              Nothing
      in
        Right (val from to) unit

    Bottom to unit ->
      let
        from =
          case prev of
            Bottom x _ ->
              Just x

            _ ->
              Nothing
      in
        Bottom (val from to) unit

    MaxHeight to unit ->
      let
        from =
          case prev of
            MaxHeight x _ ->
              Just x

            _ ->
              Nothing
      in
        MaxHeight (val from to) unit

    MaxWidth to unit ->
      let
        from =
          case prev of
            MaxWidth x _ ->
              Just x

            _ ->
              Nothing
      in
        MaxWidth (val from to) unit

    MinHeight to unit ->
      let
        from =
          case prev of
            MinHeight x _ ->
              Just x

            _ ->
              Nothing
      in
        MinHeight (val from to) unit

    MinWidth to unit ->
      let
        from =
          case prev of
            MinWidth x _ ->
              Just x

            _ ->
              Nothing
      in
        MinWidth (val from to) unit

    Padding to unit ->
      let
        from =
          case prev of
            Padding x _ ->
              Just x

            _ ->
              Nothing
      in
        Padding (val from to) unit

    PaddingLeft to unit ->
      let
        from =
          case prev of
            PaddingLeft x _ ->
              Just x

            _ ->
              Nothing
      in
        PaddingLeft (val from to) unit

    PaddingRight to unit ->
      let
        from =
          case prev of
            PaddingRight x _ ->
              Just x

            _ ->
              Nothing
      in
        PaddingRight (val from to) unit

    PaddingTop to unit ->
      let
        from =
          case prev of
            PaddingTop x _ ->
              Just x

            _ ->
              Nothing
      in
        PaddingTop (val from to) unit

    PaddingBottom to unit ->
      let
        from =
          case prev of
            PaddingBottom x _ ->
              Just x

            _ ->
              Nothing
      in
        PaddingBottom (val from to) unit

    Margin to unit ->
      let
        from =
          case prev of
            Margin x _ ->
              Just x

            _ ->
              Nothing
      in
        Margin (val from to) unit

    MarginLeft to unit ->
      let
        from =
          case prev of
            MarginLeft x _ ->
              Just x

            _ ->
              Nothing
      in
        MarginLeft (val from to) unit

    MarginRight to unit ->
      let
        from =
          case prev of
            MarginRight x _ ->
              Just x

            _ ->
              Nothing
      in
        MarginRight (val from to) unit

    MarginTop to unit ->
      let
        from =
          case prev of
            MarginTop x _ ->
              Just x

            _ ->
              Nothing
      in
        MarginTop (val from to) unit

    MarginBottom to unit ->
      let
        from =
          case prev of
            MarginBottom x _ ->
              Just x

            _ ->
              Nothing
      in
        MarginBottom (val from to) unit

    BorderWidth to unit ->
      let
        from =
          case prev of
            BorderWidth x _ ->
              Just x

            _ ->
              Nothing
      in
        BorderWidth (val from to) unit

    BorderRadius to unit ->
      let
        from =
          case prev of
            BorderRadius x _ ->
              Just x

            _ ->
              Nothing
      in
        BorderRadius (val from to) unit

    BorderTopLeftRadius to unit ->
      let
        from =
          case prev of
            BorderTopLeftRadius x _ ->
              Just x

            _ ->
              Nothing
      in
        BorderTopLeftRadius (val from to) unit

    BorderTopRightRadius to unit ->
      let
        from =
          case prev of
            BorderTopRightRadius x _ ->
              Just x

            _ ->
              Nothing
      in
        BorderTopRightRadius (val from to) unit

    BorderBottomLeftRadius to unit ->
      let
        from =
          case prev of
            BorderBottomLeftRadius x _ ->
              Just x

            _ ->
              Nothing
      in
        BorderBottomLeftRadius (val from to) unit

    BorderBottomRightRadius to unit ->
      let
        from =
          case prev of
            BorderBottomRightRadius x _ ->
              Just x

            _ ->
              Nothing
      in
        BorderBottomRightRadius (val from to) unit

    LetterSpacing to unit ->
      let
        from =
          case prev of
            LetterSpacing x _ ->
              Just x

            _ ->
              Nothing
      in
        LetterSpacing (val from to) unit

    LineHeight to unit ->
      let
        from =
          case prev of
            LineHeight x _ ->
              Just x

            _ ->
              Nothing
      in
        LineHeight (val from to) unit

    BackgroundPosition x y unit ->
      case prev of
        BackgroundPosition xFrom yFrom _ ->
          BackgroundPosition (val (Just xFrom) x) (val (Just yFrom) y) unit

        _ ->
          BackgroundPosition (val Nothing x) (val Nothing y) unit

    Color x y z a ->
      let
        ( xFrom, yFrom, zFrom, aFrom ) =
          case prev of
            Color x1 y1 z1 a1 ->
              ( Just x1, Just y1, Just z1, Just a1 )

            _ ->
              ( Nothing, Nothing, Nothing, Nothing )
      in
        Color (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a)

    BorderColor x y z a ->
      let
        ( xFrom, yFrom, zFrom, aFrom ) =
          case prev of
            BorderColor x1 y1 z1 a1 ->
              ( Just x1, Just y1, Just z1, Just a1 )

            _ ->
              ( Nothing, Nothing, Nothing, Nothing )
      in
        BorderColor (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a)

    BackgroundColor x y z a ->
      let
        ( xFrom, yFrom, zFrom, aFrom ) =
          case prev of
            BackgroundColor x1 y1 z1 a1 ->
              ( Just x1, Just y1, Just z1, Just a1 )

            _ ->
              ( Nothing, Nothing, Nothing, Nothing )
      in
        BackgroundColor (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a)

    TransformOrigin x y z unit ->
      let
        ( xFrom, yFrom, zFrom ) =
          case prev of
            TransformOrigin x1 y1 z1 _ ->
              ( Just x1, Just y1, Just z1 )

            _ ->
              ( Nothing, Nothing, Nothing )
      in
        TransformOrigin (val xFrom x) (val yFrom y) (val zFrom z) unit

    Translate x y unit ->
      let
        ( xFrom, yFrom ) =
          case prev of
            Translate x1 y1 _ ->
              ( Just x1, Just y1 )

            _ ->
              ( Nothing, Nothing )
      in
        Translate (val xFrom x) (val yFrom y) unit

    Translate3d x y z unit ->
      let
        ( xFrom, yFrom, zFrom ) =
          case prev of
            Translate3d x1 y1 z1 _ ->
              ( Just x1, Just y1, Just z1 )

            _ ->
              ( Nothing, Nothing, Nothing )
      in
        Translate3d (val xFrom x) (val yFrom y) (val zFrom z) unit

    TranslateX to unit ->
      let
        from =
          case prev of
            TranslateX x _ ->
              Just x

            _ ->
              Nothing
      in
        TranslateX (val from to) unit

    TranslateY to unit ->
      let
        from =
          case prev of
            TranslateY x _ ->
              Just x

            _ ->
              Nothing
      in
        TranslateY (val from to) unit

    Scale to ->
      let
        from =
          case prev of
            Scale x ->
              Just x

            _ ->
              Nothing
      in
        Scale (val from to)

    Scale3d x y z ->
      let
        ( xFrom, yFrom, zFrom ) =
          case prev of
            Scale3d x1 y1 z1 ->
              ( Just x1, Just y1, Just z1 )

            _ ->
              ( Nothing, Nothing, Nothing )
      in
        Scale3d (val xFrom x) (val yFrom y) (val zFrom z)

    ScaleX to ->
      let
        from =
          case prev of
            ScaleX x ->
              Just x

            _ ->
              Nothing
      in
        ScaleX (val from to)

    ScaleY to ->
      let
        from =
          case prev of
            ScaleY x ->
              Just x

            _ ->
              Nothing
      in
        ScaleY (val from to)

    ScaleZ to ->
      let
        from =
          case prev of
            ScaleZ x ->
              Just x

            _ ->
              Nothing
      in
        ScaleZ (val from to)

    Rotate to unit ->
      let
        from =
          case prev of
            Rotate x _ ->
              Just x

            _ ->
              Nothing
      in
        Rotate (val from to) unit

    Rotate3d x y z a unit ->
      let
        ( xFrom, yFrom, zFrom, aFrom ) =
          case prev of
            Rotate3d x1 y1 z1 a1 _ ->
              ( Just x1, Just y1, Just z1, Just a1 )

            _ ->
              ( Nothing, Nothing, Nothing, Nothing )
      in
        Rotate3d (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a) unit

    RotateX to unit ->
      let
        from =
          case prev of
            RotateX x _ ->
              Just x

            _ ->
              Nothing
      in
        RotateX (val from to) unit

    RotateY to unit ->
      let
        from =
          case prev of
            RotateY x _ ->
              Just x

            _ ->
              Nothing
      in
        RotateY (val from to) unit

    Skew x y unit ->
      let
        ( xFrom, yFrom ) =
          case prev of
            Skew x y _ ->
              ( Just x, Just y )

            _ ->
              ( Nothing, Nothing )
      in
        Skew (val xFrom x) (val yFrom y) unit

    SkewX to unit ->
      let
        from =
          case prev of
            SkewX x _ ->
              Just x

            _ ->
              Nothing
      in
        SkewX (val from to) unit

    SkewY to unit ->
      let
        from =
          case prev of
            SkewY x _ ->
              Just x

            _ ->
              Nothing
      in
        SkewY (val from to) unit

    Perspective to ->
      let
        from =
          case prev of
            SkewY x _ ->
              Just x

            _ ->
              Nothing
      in
        Perspective (val from to)

    Matrix a b c x y z ->
      case prev of
        Matrix aFrom bFrom cFrom xFrom yFrom zFrom ->
          Matrix
            (val (Just aFrom) a)
            (val (Just bFrom) b)
            (val (Just cFrom) c)
            (val (Just xFrom) x)
            (val (Just yFrom) y)
            (val (Just zFrom) z)

        _ ->
          Matrix
            (val Nothing a)
            (val Nothing b)
            (val Nothing c)
            (val Nothing x)
            (val Nothing y)
            (val Nothing z)

    Matrix3d a b c d e f g h i j k l m n o p ->
      case prev of
        Matrix3d a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 o2 p2 ->
          Matrix3d
            (val (Just a2) a)
            (val (Just b2) b)
            (val (Just c2) c)
            (val (Just d2) d)
            (val (Just e2) e)
            (val (Just f2) f)
            (val (Just g2) g)
            (val (Just h2) h)
            (val (Just i2) i)
            (val (Just j2) j)
            (val (Just k2) k)
            (val (Just l2) l)
            (val (Just m2) m)
            (val (Just n2) n)
            (val (Just o2) o)
            (val (Just p2) p)

        _ ->
          Matrix3d
            (val Nothing a)
            (val Nothing b)
            (val Nothing c)
            (val Nothing d)
            (val Nothing e)
            (val Nothing f)
            (val Nothing g)
            (val Nothing h)
            (val Nothing i)
            (val Nothing j)
            (val Nothing k)
            (val Nothing l)
            (val Nothing m)
            (val Nothing n)
            (val Nothing o)
            (val Nothing p)


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
  fill (List.map 
            (mapProp toStatic)
              frame.target) style


toStatic : Physics DynamicTarget -> Static
toStatic physic =
  physic.physical.position


mapProp : (a -> b) -> StyleProperty a -> StyleProperty b
mapProp fn prop =
  case prop of
    Prop n a u ->
      Prop n (fn a) u

    Display mode -> Display mode
      --case mode of
      --  DisplayMode a p t ->
      --        Display (DisplayMode (fn a) p t)

    Opacity a ->
      Opacity (fn a)

    Height a u ->
      Height (fn a) u

    Width a u ->
      Width (fn a) u

    Left a u ->
      Left (fn a) u

    Top a u ->
      Top (fn a) u

    Right a u ->
      Right (fn a) u

    Bottom a u ->
      Bottom (fn a) u

    MaxHeight a u ->
      MaxHeight (fn a) u

    MaxWidth a u ->
      MaxWidth (fn a) u

    MinHeight a u ->
      MinHeight (fn a) u

    MinWidth a u ->
      MinWidth (fn a) u

    Padding a u ->
      Padding (fn a) u

    PaddingLeft a u ->
      PaddingLeft (fn a) u

    PaddingRight a u ->
      PaddingRight (fn a) u

    PaddingTop a u ->
      PaddingTop (fn a) u

    PaddingBottom a u ->
      PaddingBottom (fn a) u

    Margin a u ->
      Margin (fn a) u

    MarginLeft a u ->
      MarginLeft (fn a) u

    MarginRight a u ->
      MarginRight (fn a) u

    MarginTop a u ->
      MarginTop (fn a) u

    MarginBottom a u ->
      MarginBottom (fn a) u

    BorderWidth a u ->
      BorderWidth (fn a) u

    BorderRadius a u ->
      BorderRadius (fn a) u

    BorderTopLeftRadius a u ->
      BorderTopLeftRadius (fn a) u

    BorderTopRightRadius a u ->
      BorderTopRightRadius (fn a) u

    BorderBottomLeftRadius a u ->
      BorderBottomLeftRadius (fn a) u

    BorderBottomRightRadius a u ->
      BorderBottomRightRadius (fn a) u

    LetterSpacing a u ->
      LetterSpacing (fn a) u

    LineHeight a u ->
      LineHeight (fn a) u

    BackgroundPosition x y u ->
      BackgroundPosition (fn x) (fn y) u

    TransformOrigin x y z u ->
      TransformOrigin (fn x) (fn y) (fn z) u

    Color x y z a ->
      Color (fn x) (fn y) (fn z) (fn a)

    BackgroundColor x y z a ->
      BackgroundColor (fn x) (fn y) (fn z) (fn a)

    BorderColor x y z a ->
      BorderColor (fn x) (fn y) (fn z) (fn a)

    Translate a1 a2 u ->
      Translate (fn a1) (fn a2) u

    Translate3d a1 a2 a3 u ->
      Translate3d (fn a1) (fn a2) (fn a3) u

    TranslateX a u ->
      TranslateX (fn a) u

    TranslateY a u ->
      TranslateY (fn a) u

    Scale a ->
      Scale (fn a)

    Scale3d a1 a2 a3 ->
      Scale3d (fn a1) (fn a2) (fn a3)

    ScaleX a ->
      ScaleX (fn a)

    ScaleY a ->
      ScaleY (fn a)

    ScaleZ a ->
      ScaleZ (fn a)

    Rotate a u ->
      Rotate (fn a) u

    Rotate3d a1 a2 a3 a4 u ->
      Rotate3d (fn a1) (fn a2) (fn a3) (fn a4) u

    RotateX a u ->
      RotateX (fn a) u

    RotateY a u ->
      RotateY (fn a) u

    Skew a1 a2 u ->
      Skew (fn a1) (fn a2) u

    SkewX a u ->
      SkewX (fn a) u

    SkewY a u ->
      SkewY (fn a) u

    Perspective a ->
      Perspective (fn a)

    Matrix a b c x y z ->
      Matrix (fn a) (fn b) (fn c) (fn x) (fn y) (fn z)

    Matrix3d a b c d e f g h i j k l m n o p ->
      Matrix3d (fn a) (fn b) (fn c) (fn d) (fn e) (fn f) (fn g) (fn h) (fn i) (fn j) (fn k) (fn l) (fn m) (fn n) (fn o) (fn p)


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
      Render.id a == Render.id b
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
      Render.debugName a == Render.debugName b
  in
    findBy (matchPropID prop) state


countOccurance x pool =
  List.foldl
    (\x2 count ->
      if Render.id x == Render.id x2 then
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
      -- need to know the Render.id of x, meaning how many times it's shown up already.
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
