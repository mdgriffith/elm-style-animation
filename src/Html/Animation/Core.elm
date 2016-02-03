module Html.Animation.Core (Model, Action (..), StyleKeyframe, Style, Physics, DynamicTarget, update, step, mapProp, bake) where

import Time exposing (Time, second)
import Effects exposing (Effects)
import Html.Animation.Properties exposing (..)
import Html.Animation.Spring as Spring
import Html.Animation.Render as Render


type alias Model =
  { start : Maybe Time
  , elapsed : Time
  , anim : List StyleKeyframe
  , previous : Style
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


type alias Physics a =
  { target : a
  , position : Float
  , spring : Spring.Model
  , easing : Maybe Easing
  }



type alias Easing =
  { ease : Float -> Float
  , counter : Maybe Spring.Model
  , duration : Time
  }




update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Queue anims ->
      ( { model | anim = model.anim ++ anims }
      , Effects.tick Tick
      )

    Interrupt anims ->
      -- Only interrupt if anims end in different states.
      --if equivalentAnim model.previous model.anim anims then
      --    ( A model, Effects.none )
      --else
      let
        currentAnim =
          List.head model.anim

        previous =
          case currentAnim of
            Nothing ->
              model.previous

            Just frame ->
              bake frame model.previous
      in
        ( { model
              | anim = mapTo 0 (\a -> step a previous 0.0 0.0) anims
              , elapsed = 0.0
              , start = Nothing
              , previous = previous
            }
        , Effects.tick Tick
        )

    Tick now ->
      let
        start =
          case model.start of
            Nothing ->
              now

            Just t ->
              t

        elapsed =
          now - start

        currentAnim =
          List.head model.anim

        remaining =
          List.tail model.anim
      in
        case currentAnim of
          Nothing ->
            ( { model
                  | elapsed = 0.0
                  , start = Nothing
                  , previous = model.previous
                  , anim = model.anim
                }
            , Effects.none
            )

          Just current ->
            let
              animElapsed =
                elapsed - current.delay
            in
              if animElapsed >= 0.0 && done animElapsed current then
                let
                  anims =
                    case remaining of
                      Nothing ->
                        []

                      Just a ->
                        a

                  previous =
                    bake current model.previous

                  resetElapsed =
                    elapsed

                
                  --newElapsed - (current.duration + current.delay)
                in
                  (  { model
                        | elapsed = 0.0
                        , start = Just now
                        , previous = previous
                        , anim = mapTo 0 (\a -> step a previous 0.0 0.0) anims
                        --, anim = Debug.log "finished" <| mapTo 0 (\a -> step a previous resetElapsed resetElapsed) anims
                      }
                  , Effects.tick Tick
                  )
              else if animElapsed >= 0.0 then
                (  { model
                      | elapsed = elapsed
                      , start = Just start
                      , anim = mapTo 0 (\a -> step a model.previous animElapsed (elapsed - model.elapsed)) model.anim
                    }
                , Effects.tick Tick
                )
              else
                ( { model
                      | elapsed = elapsed
                      , start = Just start
                      , anim = model.anim
                    }
                , Effects.tick Tick
                )



done : Time -> StyleKeyframe -> Bool
done time frame =
  List.all (propDone time) frame.target


propDone : Time -> StyleProperty (Physics DynamicTarget) -> Bool
propDone time prop =
  let
    isDone prop =
      case prop.easing of
        Nothing ->
          Spring.atRest prop.spring

        Just easing ->
          easing.ease time
            == 1.0
            && easing.counter
            == Nothing
  in
    case prop of
      Prop _ a _ ->
        isDone a

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




step : StyleKeyframe -> Style -> Time -> Time -> StyleKeyframe
step frame prev current dt =
  let
    style =
      List.foldl
        (\x acc ->
          -- need to know how many times x has shown up already.
          let
            xI =
              List.foldl
                (\x2 count ->
                  if Render.id x == Render.id x2 then
                    count + 1
                  else
                    count
                )
                0
                acc
          in
            case findProp prev x xI of
              Nothing ->
                acc

              Just prevX ->
                acc ++ [ stepProp x prevX current dt ]
        )
        []
        frame.target
  in
    { frame | target = style }


stepProp : StyleProperty (Physics DynamicTarget) -> StyleProperty Static -> Time -> Time -> StyleProperty (Physics DynamicTarget)
stepProp prop prev current dt =
  let
    val from physics =
      case physics.easing of
        Nothing ->
          let
            --target =
              --physics.target from 1.0

            --initialPos = physics.target from 0.0

            newSpring = physics.spring

            pos = 
                -- HAAAAACK, Oh lord this is a hack :/
                if current == 0.0 && dt == 0.0 then
                    from
                else
                    physics.spring.position


            targetedSpring = { newSpring | destination = physics.target from 1.0 
                                         , position = pos }

            finalSpring = 
              Spring.update dt targetedSpring
          in
            { physics
              | spring = finalSpring
              , position = finalSpring.position
            }

        Just easing ->
          let
            eased =
              easing.ease (current / easing.duration)

            position =
              physics.target from eased
          in
            physics
  in
    case prop of
      Prop name to unit ->
        let
          from =
            case prev of
              Prop _ x _ ->
                x

              _ ->
                0.0
        in
          Prop name (val from to) unit

      Opacity to ->
        let
          from =
            case prev of
              Opacity x ->
                x

              _ ->
                0.0
        in
          Opacity (val from to)

      --Opacity ()
      Height to unit ->
        let
          from =
            case prev of
              Height x _ ->
                x

              _ ->
                0.0
        in
          Height (val from to) unit

      Width to unit ->
        let
          from =
            case prev of
              Width x _ ->
                x

              _ ->
                0.0
        in
          Width (val from to) unit

      Left to unit ->
        let
          from =
            case prev of
              Left x _ ->
                x

              _ ->
                0.0
        in
          Left (val from to) unit

      Top to unit ->
        let
          from =
            case prev of
              Top x _ ->
                x

              _ ->
                0.0
        in
          Top (val from to) unit

      Right to unit ->
        let
          from =
            case prev of
              Right x _ ->
                x

              _ ->
                0.0
        in
          Right (val from to) unit

      Bottom to unit ->
        let
          from =
            case prev of
              Bottom x _ ->
                x

              _ ->
                0.0
        in
          Bottom (val from to) unit

      MaxHeight to unit ->
        let
          from =
            case prev of
              MaxHeight x _ ->
                x

              _ ->
                0.0
        in
          MaxHeight (val from to) unit

      MaxWidth to unit ->
        let
          from =
            case prev of
              MaxWidth x _ ->
                x

              _ ->
                0.0
        in
          MaxWidth (val from to) unit

      MinHeight to unit ->
        let
          from =
            case prev of
              MinHeight x _ ->
                x

              _ ->
                0.0
        in
          MinHeight (val from to) unit

      MinWidth to unit ->
        let
          from =
            case prev of
              MinWidth x _ ->
                x

              _ ->
                0.0
        in
          MinWidth (val from to) unit

      Padding to unit ->
        let
          from =
            case prev of
              Padding x _ ->
                x

              _ ->
                0.0
        in
          Padding (val from to) unit

      PaddingLeft to unit ->
        let
          from =
            case prev of
              PaddingLeft x _ ->
                x

              _ ->
                0.0
        in
          PaddingLeft (val from to) unit

      PaddingRight to unit ->
        let
          from =
            case prev of
              PaddingRight x _ ->
                x

              _ ->
                0.0
        in
          PaddingRight (val from to) unit

      PaddingTop to unit ->
        let
          from =
            case prev of
              PaddingTop x _ ->
                x

              _ ->
                0.0
        in
          PaddingTop (val from to) unit

      PaddingBottom to unit ->
        let
          from =
            case prev of
              PaddingBottom x _ ->
                x

              _ ->
                0.0
        in
          PaddingBottom (val from to) unit

      Margin to unit ->
        let
          from =
            case prev of
              Margin x _ ->
                x

              _ ->
                0.0
        in
          Margin (val from to) unit

      MarginLeft to unit ->
        let
          from =
            case prev of
              MarginLeft x _ ->
                x

              _ ->
                0.0
        in
          MarginLeft (val from to) unit

      MarginRight to unit ->
        let
          from =
            case prev of
              MarginRight x _ ->
                x

              _ ->
                0.0
        in
          MarginRight (val from to) unit

      MarginTop to unit ->
        let
          from =
            case prev of
              MarginTop x _ ->
                x

              _ ->
                0.0
        in
          MarginTop (val from to) unit

      MarginBottom to unit ->
        let
          from =
            case prev of
              MarginBottom x _ ->
                x

              _ ->
                0.0
        in
          MarginBottom (val from to) unit

      BorderWidth to unit ->
        let
          from =
            case prev of
              BorderWidth x _ ->
                x

              _ ->
                0.0
        in
          BorderWidth (val from to) unit

      BorderRadius to unit ->
        let
          from =
            case prev of
              BorderRadius x _ ->
                x

              _ ->
                0.0
        in
          BorderRadius (val from to) unit

      BorderTopLeftRadius to unit ->
        let
          from =
            case prev of
              BorderTopLeftRadius x _ ->
                x

              _ ->
                0.0
        in
          BorderTopLeftRadius (val from to) unit

      BorderTopRightRadius to unit ->
        let
          from =
            case prev of
              BorderTopRightRadius x _ ->
                x

              _ ->
                0.0
        in
          BorderTopRightRadius (val from to) unit

      BorderBottomLeftRadius to unit ->
        let
          from =
            case prev of
              BorderBottomLeftRadius x _ ->
                x

              _ ->
                0.0
        in
          BorderBottomLeftRadius (val from to) unit

      BorderBottomRightRadius to unit ->
        let
          from =
            case prev of
              BorderBottomRightRadius x _ ->
                x

              _ ->
                0.0
        in
          BorderBottomRightRadius (val from to) unit

      LetterSpacing to unit ->
        let
          from =
            case prev of
              LetterSpacing x _ ->
                x

              _ ->
                0.0
        in
          LetterSpacing (val from to) unit

      LineHeight to unit ->
        let
          from =
            case prev of
              LineHeight x _ ->
                x

              _ ->
                0.0
        in
          LineHeight (val from to) unit

      BackgroundPosition x y unit ->
        case prev of
          BackgroundPosition xFrom yFrom _ ->
            BackgroundPosition (val xFrom x) (val yFrom y) unit

          _ ->
            BackgroundPosition (val 0.0 x) (val 0.0 y) unit

      Color x y z a ->
        let
          ( xFrom, yFrom, zFrom, aFrom ) =
            case prev of
              Color x1 y1 z1 a1 ->
                ( x1, y1, z1, a1 )

              _ ->
                ( 0.0, 0.0, 0.0, 0.0 )
        in
          Color (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a)

      BorderColor x y z a ->
        let
          ( xFrom, yFrom, zFrom, aFrom ) =
            case prev of
              BorderColor x1 y1 z1 a1 ->
                ( x1, y1, z1, a1 )

              _ ->
                ( 0.0, 0.0, 0.0, 0.0 )
        in
          BorderColor (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a)

      BackgroundColor x y z a ->
        let
          ( xFrom, yFrom, zFrom, aFrom ) =
            case prev of
              BackgroundColor x1 y1 z1 a1 ->
                ( x1, y1, z1, a1 )

              _ ->
                ( 0.0, 0.0, 0.0, 0.0 )
        in
          BackgroundColor (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a)

      TransformOrigin x y z unit ->
        let
          ( xFrom, yFrom, zFrom ) =
            case prev of
              TransformOrigin x1 y1 z1 _ ->
                ( x1, y1, z1 )

              _ ->
                ( 0.0, 0.0, 0.0 )
        in
          TransformOrigin (val xFrom x) (val yFrom y) (val zFrom z) unit

      Translate x y unit ->
        let
          ( xFrom, yFrom ) =
            case prev of
              Translate x1 y1 _ ->
                ( x1, y1 )

              _ ->
                ( 0.0, 0.0 )
        in
          Translate (val xFrom x) (val yFrom y) unit

      Translate3d x y z unit ->
        let
          ( xFrom, yFrom, zFrom ) =
            case prev of
              Translate3d x1 y1 z1 _ ->
                ( x1, y1, z1 )

              _ ->
                ( 0.0, 0.0, 0.0 )
        in
          Translate3d (val xFrom x) (val yFrom y) (val zFrom z) unit

      TranslateX to unit ->
        let
          from =
            case prev of
              TranslateX x _ ->
                x

              _ ->
                0.0
        in
          TranslateX (val from to) unit

      TranslateY to unit ->
        let
          from =
            case prev of
              TranslateY x _ ->
                x

              _ ->
                0.0
        in
          TranslateY (val from to) unit

      Scale to ->
        let
          from =
            case prev of
              Scale x ->
                x

              _ ->
                0.0
        in
          Scale (val from to)

      Scale3d x y z ->
        let
          ( xFrom, yFrom, zFrom ) =
            case prev of
              Scale3d x1 y1 z1 ->
                ( x1, y1, z1 )

              _ ->
                ( 0.0, 0.0, 0.0 )
        in
          Scale3d (val xFrom x) (val yFrom y) (val zFrom z)

      ScaleX to ->
        let
          from =
            case prev of
              ScaleX x ->
                x

              _ ->
                0.0
        in
          ScaleX (val from to)

      ScaleY to ->
        let
          from =
            case prev of
              ScaleY x ->
                x

              _ ->
                0.0
        in
          ScaleY (val from to)

      ScaleZ to ->
        let
          from =
            case prev of
              ScaleZ x ->
                x

              _ ->
                0.0
        in
          ScaleZ (val from to)

      Rotate to unit ->
        let
          from =
            case prev of
              Rotate x _ ->
                x

              _ ->
                0.0
        in
          Rotate (val from to) unit

      Rotate3d x y z a unit ->
        let
          ( xFrom, yFrom, zFrom, aFrom ) =
            case prev of
              Rotate3d x1 y1 z1 a1 _ ->
                ( x1, y1, z1, a1 )

              _ ->
                ( 0.0, 0.0, 0.0, 0.0 )
        in
          Rotate3d (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a) unit

      RotateX to unit ->
        let
          from =
            case prev of
              RotateX x _ ->
                x

              _ ->
                0.0
        in
          RotateX (val from to) unit

      RotateY to unit ->
        let
          from =
            case prev of
              RotateY x _ ->
                x

              _ ->
                0.0
        in
          RotateY (val from to) unit

      Skew x y unit ->
        let
          ( xFrom, yFrom ) =
            case prev of
              Skew x y _ ->
                ( x, y )

              _ ->
                ( 0.0, 0.0 )
        in
          Skew (val xFrom x) (val yFrom y) unit

      SkewX to unit ->
        let
          from =
            case prev of
              SkewX x _ ->
                x

              _ ->
                0.0
        in
          SkewX (val from to) unit

      SkewY to unit ->
        let
          from =
            case prev of
              SkewY x _ ->
                x

              _ ->
                0.0
        in
          SkewY (val from to) unit

      Perspective to ->
        let
          from =
            case prev of
              SkewY x _ ->
                x

              _ ->
                0.0
        in
          Perspective (val from to)

      Matrix a b c x y z ->
        case prev of
          Matrix aFrom bFrom cFrom xFrom yFrom zFrom ->
            Matrix
              (val aFrom a)
              (val bFrom b)
              (val cFrom c)
              (val xFrom x)
              (val yFrom y)
              (val zFrom z)

          _ ->
            Matrix
              (val 0.0 a)
              (val 0.0 b)
              (val 0.0 c)
              (val 0.0 x)
              (val 0.0 y)
              (val 0.0 z)

      Matrix3d a b c d e f g h i j k l m n o p ->
        case prev of
          Matrix3d a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 o2 p2 ->
            Matrix3d
              (val a2 a)
              (val b2 b)
              (val c2 c)
              (val d2 d)
              (val e2 e)
              (val f2 f)
              (val g2 g)
              (val h2 h)
              (val i2 i)
              (val j2 j)
              (val k2 k)
              (val l2 l)
              (val m2 m)
              (val n2 n)
              (val o2 o)
              (val p2 p)

          _ ->
            Matrix3d
              (val 0.0 a)
              (val 0.0 b)
              (val 0.0 c)
              (val 0.0 d)
              (val 0.0 e)
              (val 0.0 f)
              (val 0.0 g)
              (val 0.0 h)
              (val 0.0 i)
              (val 0.0 j)
              (val 0.0 k)
              (val 0.0 l)
              (val 0.0 m)
              (val 0.0 n)
              (val 0.0 o)
              (val 0.0 p)





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
  fill (List.map (mapProp toStatic) frame.target) style


toStatic : Physics DynamicTarget -> Static
toStatic physic =
  physic.position


mapProp : (a -> b) -> StyleProperty a -> StyleProperty b
mapProp fn prop =
  case prop of
    Prop n a u ->
      Prop n (fn a) u

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
findProp : Style -> StyleProperty a -> Int -> Maybe (StyleProperty Static)
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




fill : List (StyleProperty Static) -> List (StyleProperty Static) -> List (StyleProperty Static)
fill new existing =
  List.foldl
    (\x acc ->
      -- need to know the Render.id of x, meaning how many times it's shown up already.
      let
        xI =
          List.foldl
            (\x2 count ->
              if Render.id x == Render.id x2 then
                count + 1
              else
                count
            )
            0
            acc
      in
        case findProp new x xI of
          Nothing ->
            acc ++ [ x ]

          Just newX ->
            acc ++ [ newX ]
    )
    []
    existing

