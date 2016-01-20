module ElmUI where


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, second)
import Effects exposing (Effects)

import Signal exposing (Address)
import String

import Animation as A

import List



type alias Model =
            { start : Maybe Time
            , elapsed : Time
            , anim : Maybe StyleAnimation
            , previous : Style Static
            }

init : Style Static -> Model
init style = { empty | previous = style }


empty : Model
empty = { elapsed = 0.0
        , start = Nothing
        , anim = Nothing
        , previous = []
        }

emptyAnimation : StyleAnimation
emptyAnimation = { target = []
                 , duration = defaultDuration
                 , ease = defaultEasing 
                 }


defaultDuration : Float
defaultDuration = 0.4 * second

defaultEasing : Float -> Float
defaultEasing x = (1 - cos (pi*x))/2


type alias StyleAnimation =
            { target : Style Static
            , duration : Time
            , ease : (Float -> Float)
            }

type alias DynamicStyleAnimation = 
            { target : Style Transition
            , duration : Time
            , ease : (Float -> Float)
            }



to : Float -> Float -> Float
to t f = t

add : Float -> Float -> Float
add mod existing = existing + mod

minus : Float -> Float -> Float
minus mod existing = existing - mod


--(:=) : Float -> Float
--(:=) f = f

--(+=) : Float -> Float -> Float
--(+=) mod prev = prev + mod

--(-=) : Float -> Float -> Float
--(-=) mod prev = prev + mod




type alias Style a
        = List (StyleProperty a)

type alias Static = Float

type alias Transition = (Float -> Float)

--type Dynamic = Dynamic (Float -> Float) StyleProperty

type StyleProperty a
        = Prop String String a
        | Opacity a
        | Height Length a
        | Width Length a
        | Left Length a
        | Top Length a
        | Right Length a
        | Bottom Length a

        | Padding Length a
        | PaddingLeft Length a
        | PaddingRight Length a
        | PaddingTop Length a
        | PaddingBottom Length a

        | Margin Length a
        | MarginLeft Length a
        | MarginRight Length a
        | MarginTop Length a
        | MarginBottom Length a
        

        -- Transformations
        | Matrix a a a a a a 
        | Matrix3d a a a a a a a a a a a a a a a a 
        | Translate Length a a
        | Translate3d Length a a a
        | TranslateX Length a
        | TranslateY Length a
        | Scale a
        | Scale3d a a a
        | ScaleX a
        | ScaleY a
        | ScaleZ a
        | Rotate Angle a
        | Rotate3d Angle a a a a
        | RotateX Angle a
        | RotateY Angle a
        | Skew Angle a a
        | SkewX Angle a 
        | SkewY Angle a
        | Perspective a


type Length
      = Px
      | Percent
      | Rem
      | Em
      | Ex
      | Ch
      | Vh
      | Vw
      | Vmin
      | Vmax
      | Mm
      | Cm
      | In
      | Pt
      | Pc


type Angle
      = Deg
      | Grad
      | Rad
      | Turn


--type MatrixAnimation = MatrixAnimation

--type Matrix3dAnimation = Matrix3dAnimation

--matrix(n,n,n,n,n,n) Defines a 2D transformation, using a matrix of six values Play it »
--matrix3d(n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n) Defines a 3D transformation, using a 4x4 matrix of 16 values  



type Action 
        = Begin DynamicStyleAnimation
        | Tick Time



animate : DynamicStyleAnimation -> Model -> ( Model, Effects Action )
animate anims model = update (Begin anims) model


start : List (StyleProperty Transition) -> DynamicStyleAnimation
start props = { emptyAnimation | target = props} 



update : Action -> Model -> ( Model, Effects Action )
update action model = 
        case action of

          Begin dynamicAnims ->

              let
                -- Convert dynamic to static
                anims = makeStatic dynamicAnims model.previous

                previous = 
                  case model.anim of
                    Nothing -> model.previous
                    Just a -> 
                      bake model.elapsed a model.previous
              in
                ( { model | anim = Just anims
                          , elapsed = 0.0
                          , start = Nothing 
                          , previous = previous }
                , Effects.tick Tick )

          Tick now ->
            let
              start = 
                case model.start of
                  Nothing -> now
                  Just t -> t
              newElapsed = now - start

              (done, finalElapsed) =
                 case model.anim of
                    Nothing -> (False, newElapsed)
                    Just a ->
                      if newElapsed >= a.duration then
                        (True, a.duration)
                      else
                        (False, newElapsed)

            in
              if done then
                let
                   previous = 
                    case model.anim of
                      Nothing -> model.previous
                      Just a -> 
                        bake finalElapsed a model.previous
                in
                  ( { model | elapsed = finalElapsed
                            , previous = previous 
                            , start = Nothing 
                            , anim = Nothing }
                  , Effects.none )
              else
                ( { model | elapsed = finalElapsed 
                          , start = Just start }
                , Effects.tick Tick )


findFrom : Style Static -> StyleProperty a -> Maybe (StyleProperty Static)
findFrom state prop =
            let
              findBy fn xs = List.head (List.filter fn xs)
              matchPropID a b = propId a == propId b
            in 
              findBy (matchPropID prop) state


render : Model -> List (String, String)
render model = 
        case model.anim of
          Nothing -> 
            let
              rendered = 
                  List.map renderProp model.previous

              transformsNprops = 
                  List.partition (\s -> fst s == "transform") rendered

              combinedTransforms = ("transform", 
                    String.concat (List.intersperse " " 
                    (List.map (snd) (fst transformsNprops))))
            in
              snd transformsNprops ++ [combinedTransforms]


          Just anim ->
            -- Combine all transform properties
            let

              baked = bake model.elapsed anim model.previous

              rendered = 
                  List.map renderProp baked

              transformsNprops = 
                  List.partition (\s -> fst s == "transform") rendered

              combinedTransforms = ("transform", 
                    String.concat (List.intersperse " " 
                    (List.map (snd) (fst transformsNprops))))
            in
              snd transformsNprops ++ [combinedTransforms]
               


renderProp : StyleProperty Static -> (String, String)
renderProp prop = ( renderName prop 
                  , renderValue prop
                  )


renderName : StyleProperty Static -> String
renderName styleProp = 
            case styleProp of
              Prop str _ _-> str

              Opacity _   -> "opacity"
              Height _ _  -> "height"
              Width _ _   -> "width"
              Left _ _    -> "left"
              Right _ _   -> "right"
              Bottom _ _  -> "bottom"
              Top _ _     -> "top"

              Padding _ _       -> "padding"
              PaddingLeft _ _   -> "padding-left"
              PaddingRight _ _  -> "padding-right"
              PaddingTop _ _    -> "padding-top"
              PaddingBottom _ _ -> "padding-bottom"

              Margin _ _       -> "margin"
              MarginLeft _ _   -> "margin-left"
              MarginRight _ _  -> "margin-right"
              MarginTop _ _    -> "margin-top"
              MarginBottom _ _ -> "margin-bottom"

              Matrix _ _ _ _ _ _ -> "transform"
              Matrix3d _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ -> "transform"
              Translate _ _ _ -> "transform"
              Translate3d _ _ _ _ ->"transform"
              TranslateX _ _-> "transform"
              TranslateY _ _-> "transform"
              Scale _ -> "transform"
              Scale3d _ _ _ -> "transform"
              ScaleX _ -> "transform"
              ScaleY _ -> "transform"
              ScaleZ _ -> "transform"
              Rotate _ _ -> "transform"
              Rotate3d _ _ _ _ _ -> "transform"
              RotateX _ _ -> "transform"
              RotateY _ _ -> "transform"
              Skew _ _ _  -> "transform"
              SkewX _ _ -> "transform"
              SkewY _ _ -> "transform"
              Perspective _ -> "transform"





makeStatic : DynamicStyleAnimation -> Style Static -> StyleAnimation
makeStatic dynamic previous = 
                let
                  from prop = findFrom previous prop 
                  fn fr dynamicTo = dynamicTo fr
                in
                  { duration = dynamic.duration
                  , ease = dynamic.ease
                  , target = List.map (\p -> bakeProp p (from p) fn) dynamic.target
                  }





bake : Time -> StyleAnimation -> Style Static -> Style Static
bake elapsed anim prev = 
          let
            percentComplete = elapsed / anim.duration
            eased = anim.ease percentComplete
            from prop = findFrom prev prop 
            fn fr to = ((to-fr) * eased) + fr
          in
            List.map (\p -> bakeProp p (from p) fn) anim.target





bakeProp : StyleProperty a ->  Maybe (StyleProperty Static) -> (Float -> a -> c) -> StyleProperty c
bakeProp prop prev val =
            --let
            --  val from to = ((to-from) * percentComplete) + from
            --in
              case prop of
                Prop name unit to -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (Prop _ _ x) -> x
                        _ -> 0.0
                  in
                    Prop name unit (val from to)


                Opacity to -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (Opacity x) -> x
                        _ -> 0.0
                  in
                    Opacity (val from to)


                Height unit to -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (Height _ x) -> x
                        _ -> 0.0
                  in
                    Height unit (val from to)


                Width unit to -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (Width _ x) -> x
                        _ -> 0.0
                  in
                    Width unit (val from to)


                Left unit to -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (Left _ x) -> x
                        _ -> 0.0
                  in
                    Left unit (val from to)


                Top unit to -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (Top _ x) -> x
                        _ -> 0.0
                  in
                    Top unit (val from to)


                Right unit to ->  
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (Right _ x) -> x
                        _ -> 0.0
                  in
                    Right unit (val from to)


                Bottom unit to -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (Bottom _ x) -> x
                        _ -> 0.0
                  in
                    Bottom unit (val from to)


                Padding unit to -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (Padding _ x) -> x
                        _ -> 0.0
                  in
                    Padding unit (val from to)


                PaddingLeft unit to -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (PaddingLeft _ x) -> x
                        _ -> 0.0
                  in
                    PaddingLeft unit (val from to)


                PaddingRight unit to  -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (PaddingRight _ x) -> x
                        _ -> 0.0
                  in
                    PaddingRight unit (val from to)


                PaddingTop unit to -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (PaddingTop _ x) -> x
                        _ -> 0.0
                  in
                    PaddingTop unit (val from to)


                PaddingBottom unit to -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (PaddingBottom _ x) -> x
                        _ -> 0.0
                  in
                    PaddingBottom unit (val from to)


                Margin unit to ->
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (Margin _ x) -> x
                        _ -> 0.0
                  in
                    Margin unit (val from to)


                MarginLeft unit to   -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (MarginLeft _ x) -> x
                        _ -> 0.0
                  in
                    MarginLeft unit (val from to)


                MarginRight unit to  -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (MarginRight _ x) -> x
                        _ -> 0.0
                  in
                    MarginRight unit (val from to)


                MarginTop unit to -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (MarginTop _ x) -> x
                        _ -> 0.0
                  in
                    MarginTop unit (val from to) 


                MarginBottom unit to ->  
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (MarginBottom _ x) -> x
                        _ -> 0.0
                  in
                    MarginBottom unit (val from to)


                Translate unit x y -> 
                  let
                    (xFrom, yFrom) =
                      case prev of
                        Nothing -> (0.0, 0.0)
                        Just (Translate _ x1 y1) -> (x1, y1)
                        _ -> (0.0, 0.0)
                  in
                    Translate unit (val xFrom x) (val yFrom y)
                    

                Translate3d unit x y z -> 
                  let
                    (xFrom, yFrom, zFrom) =
                      case prev of
                        Nothing -> (0.0, 0.0, 0.0)
                        Just (Translate3d _ x1 y1 z1) -> (x1, y1, z1)
                        _ -> (0.0, 0.0, 0.0)
                  in
                    Translate3d unit (val xFrom x) (val yFrom y) (val zFrom z)
                  

                TranslateX unit to -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (TranslateX _ x) -> x
                        _ -> 0.0
                  in
                    TranslateX unit (val from to)


                TranslateY unit to -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (TranslateY _ x) -> x
                        _ -> 0.0
                  in
                    TranslateY unit (val from to)


                Scale to -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (Scale x) -> x
                        _ -> 0.0
                  in
                     Scale (val from to)


                Scale3d x y z -> 
                  let
                    (xFrom, yFrom, zFrom) =
                      case prev of
                        Nothing -> (0.0, 0.0, 0.0)
                        Just (Scale3d x1 y1 z1) -> (x1, y1, z1)
                        _ -> (0.0, 0.0, 0.0)
                  in
                    Scale3d (val xFrom x) (val yFrom y) (val zFrom z)
                   

                ScaleX to -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (ScaleX x) -> x
                        _ -> 0.0
                  in
                    ScaleX (val from to)


                ScaleY to -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (ScaleY x) -> x
                        _ -> 0.0
                  in
                    ScaleY (val from to)


                ScaleZ to -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (ScaleZ x) -> x
                        _ -> 0.0
                  in
                    ScaleZ (val from to)


                Rotate unit to -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (Rotate _ x) -> x
                        _ -> 0.0
                  in
                    Rotate unit (val from to)


                Rotate3d unit x y z a ->
                  let
                    (xFrom, yFrom, zFrom, aFrom) =
                      case prev of
                        Nothing -> (0.0, 0.0, 0.0, 0.0)
                        Just (Rotate3d _ x1 y1 z1 a1) -> (x1, y1, z1, a1)
                        _ -> (0.0, 0.0, 0.0, 0.0)
                  in 
                    Rotate3d unit (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a)
                   

                RotateX unit to -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (RotateX _ x) -> x
                        _ -> 0.0
                  in
                    RotateX unit (val from to)

                RotateY unit to -> 
                  let
                    from =
                      case prev of
                        Nothing -> 0.0
                        Just (RotateY _ x) -> x
                        _ -> 0.0
                  in
                    RotateY unit (val from to)


                Skew unit x y ->
                  let
                    (xFrom, yFrom) =
                      case prev of
                        Nothing -> (0.0, 0.0)
                        Just (Skew _ x y) -> (x, y)
                        _ -> (0.0, 0.0)
                  in
                    Skew unit (val xFrom x) (val yFrom y)
                    

                SkewX unit to -> 
                  let
                     from =
                      case prev of
                        Nothing -> 0.0
                        Just (SkewX _ x) -> x
                        _ -> 0.0
                  in
                    SkewX unit (val from to)

                SkewY unit to -> 
                  let
                     from =
                      case prev of
                        Nothing -> 0.0
                        Just (SkewY _ x) -> x
                        _ -> 0.0
                  in
                    SkewY unit (val from to)

                Perspective to -> 
                  let
                     from =
                      case prev of
                        Nothing -> 0.0
                        Just (SkewY _ x) -> x
                        _ -> 0.0
                  in
                    Perspective (val from to)

                Matrix a b c x y z -> 
                  let
                    (aFrom, bFrom, cFrom, xFrom, yFrom, zFrom) =
                      case prev of
                        Nothing -> 
                            (0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

                        Just (Matrix a2 b2 c2 x2 y2 z2) -> 
                            (a2, b2, c2, x2, y2, z2)

                        _ -> 
                            (0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
                  in 
                    Matrix (val aFrom a) (val bFrom b) (val cFrom c) (val xFrom x) (val yFrom y) (val zFrom z)
                


                Matrix3d a b c d e f g h i j k l m n o p -> 
                      case prev of
                        Nothing -> 
                            Matrix3d (val 0.0 a) (val 0.0 b) (val 0.0 c) (val 0.0 d) 
                                     (val 0.0 e) (val 0.0 f) (val 0.0 g) (val 0.0 h) 
                                     (val 0.0 i) (val 0.0 j) (val 0.0 k) (val 0.0 l) 
                                     (val 0.0 m) (val 0.0 n) (val 0.0 o) (val 0.0 p)

                        Just (Matrix3d a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 o2 p2) -> 
                             Matrix3d (val a2 a) (val b2 b) (val c2 c) (val d2 d) 
                                      (val e2 e) (val f2 f) (val g2 g) (val h2 h) 
                                      (val i2 i) (val j2 j) (val k2 k) (val l2 l) 
                                      (val m2 m) (val n2 n) (val o2 o) (val p2 p)

                        _ -> 
                             Matrix3d (val 0.0 a) (val 0.0 b) (val 0.0 c) (val 0.0 d) 
                                      (val 0.0 e) (val 0.0 f) (val 0.0 g) (val 0.0 h) 
                                      (val 0.0 i) (val 0.0 j) (val 0.0 k) (val 0.0 l) 
                                      (val 0.0 m) (val 0.0 n) (val 0.0 o) (val 0.0 p)

                   




renderValue : StyleProperty Static -> String
renderValue prop  =
            let
              val a = toString a
              renderLength unit a = addLengthUnits unit (val a)
              renderAngle unit a = addAngleUnits unit (val a)

              renderList xs = String.concat 
                              <| List.intersperse "," 
                              <| List.map toString xs
            in
              case prop of
                Prop _ u a -> (val a) ++ u

                Opacity a -> val a
                Height unit a -> renderLength unit a
                Width unit a -> renderLength unit a
                Left unit a -> renderLength unit a
                Top unit a -> renderLength unit a
                Right unit a -> renderLength unit a
                Bottom unit a -> renderLength unit a

                Padding unit a       -> renderLength unit a 
                PaddingLeft unit a   -> renderLength unit a 
                PaddingRight unit a  -> renderLength unit a
                PaddingTop unit a    -> renderLength unit a 
                PaddingBottom unit a -> renderLength unit a 

                Margin unit a       -> renderLength unit a 
                MarginLeft unit a   -> renderLength unit a 
                MarginRight unit a  -> renderLength unit a 
                MarginTop unit a    -> renderLength unit a 
                MarginBottom unit a -> renderLength unit a 

                Translate unit a1 a2 -> 
                        "translate(" ++ (renderLength unit a1) 
                              ++ "," ++ (renderLength unit a2) 
                              ++ ")"

                Translate3d unit a1 a2 a3 -> 
                          "translate3d(" ++ (renderLength unit a1) 
                                 ++ "," ++ (renderLength unit a2) 
                                 ++  "," ++ (renderLength unit a3) 
                                 ++ ")"

                TranslateX unit a -> "translateX(" ++ renderLength unit a ++ ")"
                TranslateY unit a -> "translateY(" ++ renderLength unit a ++ ")"
                Scale a1 -> "scale(" ++ (val a1)  ++ ")"
                Scale3d a1 a2 a3 -> "scale3d(" ++ (val a1) 
                                        ++ "," ++ (val a2) 
                                        ++ "," ++ (val a3) 
                                        ++ ")"
                ScaleX a -> "scaleX(" ++ val a ++ ")"
                ScaleY a -> "scaleY(" ++ val a ++ ")"
                ScaleZ a -> "scaleZ(" ++ val a ++ ")"
                Rotate unit a -> "rotate(" ++ renderAngle unit a ++ ")"
                Rotate3d unit a1 a2 a3 a4 -> 
                                          "rotate3d(" ++ (val a1) 
                                              ++ "," ++ (val a2) 
                                              ++ "," ++ (val a3) 
                                              ++ "," ++ (renderAngle unit a4) 
                                              ++ ")"

                RotateX unit a -> "rotateX(" ++ renderAngle unit a ++ ")"
                RotateY unit a -> "rotateY(" ++renderAngle unit a ++ ")"
                Skew unit a1 a2 -> 
                              "skew(" ++ (renderAngle unit a1) 
                               ++ "," ++ (renderAngle unit a2) 
                               ++ ")"
                SkewX unit a -> "skewX(" ++ renderAngle unit a ++ ")"
                SkewY unit a -> "skewY(" ++ renderAngle unit a ++ ")"
                Perspective a -> "perspective(" ++ (val a) ++ ")"

                Matrix a b c x y z -> 
                        "matrix(" ++ 
                          (renderList [a,b,c,x,y,z])
                            ++ ")"
                
                Matrix3d a b c d e f g h i j k l m n o p -> 
                        "matrix3d(" ++ 
                          (renderList [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p])
                            ++ ")"




done : Model -> Time -> Bool
done model elapsed =
        case model.anim of
          Nothing -> True
          Just a ->
            elapsed >= a.duration


propId : StyleProperty a-> Int
propId prop =
        case prop of
          Prop _ _ _ -> 1
          Opacity _  -> 2
          Height _ _ -> 3
          Width _ _  -> 4
          Left _ _   -> 5
          Right _ _  -> 6
          Bottom _ _ -> 7
          Top _ _    -> 8

          Padding _ _      -> 9
          PaddingLeft _ _  -> 10
          PaddingRight _ _ -> 11
          PaddingTop _ _   -> 12
          PaddingBottom _ _-> 13

          Margin _ _      -> 14
          MarginLeft _ _  -> 15
          MarginRight _ _ -> 16
          MarginTop _ _   -> 17
          MarginBottom _ _-> 18


          Matrix _ _ _ _ _ _ -> 36
          Matrix3d _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ -> 37
          Translate _ _ _ -> 19
          Translate3d _ _ _ _ -> 20
          TranslateX _ _ -> 21
          TranslateY _ _ -> 22
          Scale _        -> 23 
          Scale3d _ _ _  -> 24
          ScaleX _   -> 25
          ScaleY _   -> 26
          ScaleZ _   -> 27
          Rotate _ _ -> 28
          Rotate3d _ _ _ _ _ -> 29
          RotateX _ _   -> 30
          RotateY _ _   -> 31
          Skew _ _ _    -> 32 
          SkewX _ _     -> 33
          SkewY _ _     -> 34
          Perspective _ -> 35



addLengthUnits : Length -> String -> String
addLengthUnits unit num = 
          case unit of
            Px -> num ++ "px"

            Percent -> num ++ "%"

            Rem -> num ++ "rem"

            Em -> num ++ "em"
            Ex -> num ++ "ex"
            Ch -> num ++ "ch"
            Vh -> num ++ "vh"
            Vw -> num ++ "vw"
            Vmin -> num ++ "vmin"
            Vmax -> num ++ "vmax"
            Mm -> num ++ "mm"
            Cm -> num ++ "cm"
            In -> num ++ "in"
            Pt -> num ++ "pt"
            Pc -> num ++ "pc"



addAngleUnits : Angle -> String -> String
addAngleUnits unit num =
          case unit of
            Deg -> num ++ "deg"

            Grad -> num ++ "grad"

            Rad -> num ++ "rad"

            Turn -> num ++ "turn"







