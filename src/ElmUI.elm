module ElmUI where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects exposing (Effects)
import Signal exposing (Address)

import Time exposing (Time, second)

import String
import List

type alias Model =
            { start : Maybe Time
            , elapsed : Time
            , anim : Maybe (StyleAnimation Dynamic)
            , previous : Style Static
            }


type alias Static = Float


type alias Dynamic 
        = (Float -> Float -> Float)


type alias Style a
        = List (StyleProperty a)


type alias StyleAnimation a =
            { target : Style a
            , duration : Time
            , ease : (Float -> Float)
            }


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


        -- Color
        | Color ColorType a a a
        | BackgroundColor ColorType a a a

        | ColorA ColorAlphaType a a a a
        | BackgroundColorA ColorAlphaType a a a a

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

-- Units
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


type ColorType
      = RGB
      | HSL

type ColorAlphaType
        = RGBA
        | HSLA

type Action 
        = Begin (StyleAnimation Dynamic)
        | Tick Time



empty : Model
empty = { elapsed = 0.0
        , start = Nothing
        , anim = Nothing
        , previous = []
        }


initStyle : Style Static -> Model
initStyle sty = { empty | previous = sty }


emptyAnimation : StyleAnimation Static
emptyAnimation = { target = []
                 , duration = defaultDuration
                 , ease = defaultEasing 
                 }


defaultDuration : Float
defaultDuration = 0.4 * second


defaultEasing : Float -> Float
defaultEasing x = (1 - cos (pi*x))/2




update : Action -> Model -> ( Model, Effects Action )
update action model = 
        case action of

          Begin dynamicAnims ->

              let
                -- Convert dynamic to static
                --anims = makeStatic dynamicAnims model.previous

                previous = 
                  case model.anim of
                    Nothing -> model.previous
                    Just a -> 
                      bake model.elapsed a model.previous
              in
                ( { model | anim = Just dynamicAnims
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


-- Convenience Functions
animateOn : Model -> StyleAnimation Dynamic -> ( Model, Effects Action )
animateOn model anims = animate anims model

animate : StyleAnimation Dynamic -> Model -> ( Model, Effects Action )
animate anims model = update (Begin anims) model


props : List (StyleProperty Dynamic) -> StyleAnimation Dynamic
props p  = { emptyAnimation | target = p} 


duration : Time -> StyleAnimation Dynamic -> StyleAnimation Dynamic
duration dur anim = { anim | duration = dur }

easing : (Float -> Float) -> StyleAnimation Dynamic -> StyleAnimation Dynamic
easing ease anim = { anim | ease = ease }


-- Convenient function to forward an update to a style object contained in a type
-- See the Showcase Example to get an idea of whats going on here
forwardTo : Int -> List a -> (a -> Model) -> (a -> Model -> a) -> (Model -> (Model, (Effects Action))) -> (List a, Effects Action)
forwardTo i widgets styleGet styleSet fn = 
              let
                applied = 
                  List.indexedMap 
                        (\j w -> 
                            if j == i then
                              let
                                (newStyle, fx) = fn (styleGet w)
                              in
                                (styleSet w newStyle, fx)
                            else
                              (w, Effects.none)
                        ) widgets

                combineEffects ef1 ef2 =
                          if ef1 == Effects.none then
                            ef2
                          else
                            ef1
                 
              in
                 List.foldr 
                      (\x acc ->
                          case acc of
                            (ws, eff1) ->
                              case x of
                                (w, eff2) ->
                                  (w::ws, combineEffects eff1 eff2)
                      ) ([], Effects.none) applied

-- Takes 
--     * provided value
--     * previous value
--     * current time
--     * returns current value

to : Float -> Float -> Float -> Float
to target from current = ((target-from) * current) + from


add : Float -> Float -> Float -> Float
add mod from current = 
        let
          target = from + mod
        in
          to target from current


minus : Float -> Float -> Float -> Float
minus mod from current = 
        let
          target = from - mod
        in
          to target from current


(:=) : Float -> Float -> Float -> Float
(:=) t f c = to t f c

(+=) : Float -> Float -> Float -> Float
(+=) t f c = add t f c

(-=) : Float -> Float -> Float -> Float
(-=) t f c = minus t f c



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


renderName : StyleProperty a -> String
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

              Color _ _ _ _    -> "color"
              BackgroundColor _ _ _ _ -> "background-color"

              ColorA _ _ _ _ _ -> "color"
              BackgroundColorA _ _ _ _ _ -> "background-color"


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


fill : List (StyleProperty Static) -> List (StyleProperty Static) -> List (StyleProperty Static)
fill new existing =
           List.foldl
                    (\x acc ->
                      case findFrom acc x of
                        Nothing -> x::acc 
                        Just _ -> acc
                    ) new existing


bake : Time -> StyleAnimation Dynamic -> Style Static -> Style Static
bake elapsed anim prev = 
          let
            percentComplete =
                 elapsed / anim.duration

            eased = 
                anim.ease percentComplete

            style = 
                List.map 
                  (\p -> bakeProp p (findFrom prev p) eased) 
                    anim.target
          in
            -- If properties are in previous
            -- but not in the current animation
            -- copy them over as is
            fill style prev


bakeProp : StyleProperty Dynamic -> Maybe (StyleProperty Static) -> Float -> StyleProperty Static
bakeProp prop prev current =
            let
              val from fn = fn from current
            in
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


                Color unit x y z    -> 
                  let
                    (xFrom, yFrom, zFrom) =
                      case prev of
                        Nothing -> (0.0, 0.0, 0.0)
                        Just (Color _ x1 y1 z1) -> (x1, y1, z1)
                        _ -> (0.0, 0.0, 0.0)
                  in
                    Color unit (val xFrom x) (val yFrom y) (val zFrom z)


                BackgroundColor unit x y z -> 
                  let
                    (xFrom, yFrom, zFrom) =
                      case prev of
                        Nothing -> (0.0, 0.0, 0.0)
                        Just (BackgroundColor _ x1 y1 z1) -> (x1, y1, z1)
                        _ -> (0.0, 0.0, 0.0)
                  in
                    BackgroundColor unit (val xFrom x) (val yFrom y) (val zFrom z)


                ColorA unit x y z a -> 
                  let
                    (xFrom, yFrom, zFrom, aFrom) =
                      case prev of
                        Nothing -> (0.0, 0.0, 0.0, 0.0)
                        Just (ColorA _ x1 y1 z1 a1) -> (x1, y1, z1, a1)
                        _ -> (0.0, 0.0, 0.0, 0.0)
                  in
                    ColorA unit (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a)


                BackgroundColorA unit x y z a -> 
                  let
                    (xFrom, yFrom, zFrom, aFrom) =
                      case prev of
                        Nothing -> (0.0, 0.0, 0.0, 0.0)
                        Just (BackgroundColorA _ x1 y1 z1 a1) -> (x1, y1, z1, a1)
                        _ -> (0.0, 0.0, 0.0, 0.0)
                  in
                    BackgroundColorA unit (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a)



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
              renderLength unit a = (val a) ++ lenUnit unit
              renderAngle unit a =  (val a) ++ angleUnit unit
              renderList xs = "(" ++ (String.concat 
                              <| List.intersperse "," 
                              <| List.map toString xs) ++ ")"

              renderIntList xs = renderList <| List.map round xs

              renderColorA xyza =
                  let
                    int a = toString (round a)
                  in
                    case xyza of
                      (x,y,z,a) ->
                        "(" ++ int x ++ 
                        "," ++ int y ++ 
                        "," ++ int z ++ 
                        "," ++ toString a ++ ")"

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

                Color unit x y z    -> 
                      (colorUnit unit) ++ renderIntList [x,y,z]

                BackgroundColor unit x y z -> 
                       (colorUnit unit) ++ renderIntList [x,y,z]

                ColorA unit x y z a -> 
                      (colorAUnit unit) ++ renderColorA (x,y,z,a)

                BackgroundColorA unit x y z a -> 
                      (colorAUnit unit) ++ renderColorA (x,y,z,a)


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
                        "matrix" ++ 
                          (renderList [a,b,c,x,y,z])
                        
                
                Matrix3d a b c d e f g h i j k l m n o p -> 
                        "matrix3d" ++ 
                          (renderList [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p])
                           


done : Model -> Time -> Bool
done model elapsed =
        case model.anim of
          Nothing -> True
          Just a ->
            elapsed >= a.duration


propId : StyleProperty a -> String
propId prop =
        case prop of
          Prop name unit _ -> name ++ unit
          Opacity _  -> "opacity"
          Height unit _ -> "height" ++ (lenUnit unit)
          Width unit _  -> "width" ++ (lenUnit unit)
          Left unit _   -> "left" ++ (lenUnit unit)
          Right unit _  -> "right" ++ (lenUnit unit)
          Bottom unit _ -> "bottom" ++ (lenUnit unit)
          Top unit _    -> "top" ++ (lenUnit unit)

          Padding unit _      -> "padding" ++ (lenUnit unit)
          PaddingLeft unit _  -> "padding-left" ++ (lenUnit unit)
          PaddingRight unit _ -> "padding-right" ++ (lenUnit unit)
          PaddingTop unit _   -> "padding-top" ++ (lenUnit unit)
          PaddingBottom unit _-> "padding-bottom" ++ (lenUnit unit)

          Margin unit _      -> "margin" ++ (lenUnit unit)
          MarginLeft unit _  -> "margin-left" ++ (lenUnit unit)
          MarginRight unit _ -> "margin-right" ++ (lenUnit unit)
          MarginTop unit _   -> "margin-top" ++ (lenUnit unit)
          MarginBottom unit _-> "margin-bottom" ++ (lenUnit unit)

          Color unit _ _ _    -> "color" ++ (colorUnit unit)
          BackgroundColor unit _ _ _ -> "background-color" ++ (colorUnit unit)

          ColorA unit _ _ _ _ -> "color" ++ (colorAUnit unit)
          BackgroundColorA unit _ _ _ _ -> "background-color" ++ (colorAUnit unit)

          Matrix _ _ _ _ _ _ -> "matrix"
          Matrix3d _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ -> "matrix3d"
          Translate unit _ _ -> "translate" ++ (lenUnit unit)
          Translate3d unit _ _ _ -> "translate3d"  ++ (lenUnit unit)
          TranslateX unit _ -> "translatex" ++ (lenUnit unit)
          TranslateY unit _ -> "translatey" ++ (lenUnit unit)
          Scale _        -> "scale"
          Scale3d _ _ _  -> "scale3d"
          ScaleX _   -> "scalex"
          ScaleY _   -> "scaley"
          ScaleZ _   -> "scalez"
          Rotate unit _ -> "rotate" ++ (angleUnit unit)
          Rotate3d unit _ _ _ _ -> "rotate3d" ++ (angleUnit unit)
          RotateX unit _   -> "rotatex" ++ (angleUnit unit)
          RotateY unit _   -> "rotatey" ++ (angleUnit unit)
          Skew unit _ _    -> "skew" ++ (angleUnit unit)
          SkewX unit _     -> "skewx" ++ (angleUnit unit)
          SkewY unit _     -> "skewy" ++ (angleUnit unit)
          Perspective _ -> "perspective"




colorUnit : ColorType -> String
colorUnit color =
            case color of
              RGB -> "rgb"
              HSL -> "hsl"

colorAUnit : ColorAlphaType -> String
colorAUnit color =
            case color of
              RGBA -> "rgba"
              HSLA -> "hsla"


lenUnit : Length -> String
lenUnit unit = 
          case unit of
            Px -> "px"
            Percent -> "%"
            Rem -> "rem"
            Em -> "em"
            Ex -> "ex"
            Ch -> "ch"
            Vh -> "vh"
            Vw -> "vw"
            Vmin -> "vmin"
            Vmax -> "vmax"
            Mm -> "mm"
            Cm -> "cm"
            In -> "in"
            Pt -> "pt"
            Pc -> "pc"


angleUnit : Angle -> String
angleUnit unit = 
          case unit of
            Deg -> "deg"
            Grad -> "grad"
            Rad -> "rad"
            Turn -> "turn"








