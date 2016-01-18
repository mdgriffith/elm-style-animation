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


type alias Model = { start : Maybe Time 
                   , elapsed : Time
                   , anim : Maybe (List PropAnimation)
                   }


type PropAnimation
        = Prop String String A.Animation
        | Opacity A.Animation
        | Height DistanceUnit A.Animation
        | Width DistanceUnit A.Animation
        | Left DistanceUnit A.Animation
        | Top DistanceUnit A.Animation
        | Right DistanceUnit A.Animation
        | Bottom DistanceUnit A.Animation

        --| PaddingLeft DistanceUnit
        

        -- Transformations
--        --| Matrix --Float Float Float Float Float Float 
--        --| Matrix3d -- Float Float Float Float Float Float Float Float Float Float Float Float Float Float Float Float 
        | Translate DistanceUnit A.Animation A.Animation
        | Translate3d DistanceUnit A.Animation A.Animation A.Animation
        | TranslateX DistanceUnit A.Animation
        | TranslateY DistanceUnit A.Animation
        | Scale A.Animation
        | Scale3d A.Animation A.Animation A.Animation
        | ScaleX A.Animation
        | ScaleY A.Animation
        | ScaleZ A.Animation
        | Rotate RotationUnit A.Animation
        | Rotate3d RotationUnit A.Animation A.Animation A.Animation A.Animation
        | RotateX RotationUnit A.Animation
        | RotateY RotationUnit A.Animation
        | Skew RotationUnit A.Animation A.Animation
        | SkewX RotationUnit A.Animation 
        | SkewY RotationUnit A.Animation
        | Perspective A.Animation

type DistanceUnit
      = Px
      | Percent
      | Rem
      | Em

type RotationUnit
      = Deg
      | Grad
      | Rad
      | Turn


--type MatrixAnimation = MatrixAnimation

--type Matrix3dAnimation = Matrix3dAnimation

--matrix(n,n,n,n,n,n) Defines a 2D transformation, using a matrix of six values Play it »
--matrix3d(n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n) Defines a 3D transformation, using a 4x4 matrix of 16 values  



empty : Model
empty = { elapsed = 1.0*second
        , start = Nothing
        , anim = Nothing
        }


type Action 
        = Begin (List PropAnimation)
        | Tick Time



render : Model -> List (String, String)
render model = 
            case model.anim of
              Nothing -> []
              Just anim ->
                -- Combine all transform properties
                let
                  rendered = 
                      List.map (renderProp model.elapsed) anim

                  transformsNprops = 
                      List.partition (\s -> fst s == "transform") rendered

                  combinedTransforms = ("transform", 
                        String.concat (List.intersperse " " 
                        (List.map (snd) (fst transformsNprops))))
                in
                  snd transformsNprops ++ [combinedTransforms]
               
                

renderProp : Time -> PropAnimation -> (String, String)
renderProp elapsed propAnim =
                  ( renderName propAnim
                  , renderValue propAnim elapsed
                  )

renderName : PropAnimation -> String
renderName propAnim = 
            case propAnim of
              Prop str _ _-> str

              Opacity _   -> "opacity"
              Height _ _  -> "height"
              Width _ _   -> "width"
              Left _ _    -> "left"
              Right _ _   -> "right"
              Bottom _ _  -> "bottom"
              Top _ _     -> "top"

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


renderValue : PropAnimation -> Float -> String
renderValue prop elapsed =
            let
              val a = toString (A.animate elapsed a)
              dval unit a = dUnit unit (val a)
              rval unit a = rUnit unit (val a)
            in
              case prop of
                Opacity a -> val a
                Height unit a -> dval unit a
                Width unit a -> dval unit a
                Left unit a -> dval unit a
                Top unit a -> dval unit a
                Right unit a -> dval unit a
                Bottom unit a -> dval unit a
                Prop _ u a -> (val a) ++ u

                Translate unit a1 a2 -> "translate(" ++ (dval unit a1) 
                                              ++ "," ++ (dval unit a2) 
                                              ++ ")"
                Translate3d unit a1 a2 a3 -> "translate3d(" ++ (dval unit a1) 
                                                     ++ "," ++ (dval unit a2) 
                                                     ++  "," ++ (dval unit a3) 
                                                     ++ ")"
                TranslateX unit a -> "translateX(" ++ dval unit a ++ ")"
                TranslateY unit a -> "translateY(" ++ dval unit a ++ ")"
                Scale a1 -> "scale(" ++ (val a1)  ++ ")"
                Scale3d a1 a2 a3 -> "scale3d(" ++ (val a1) 
                                        ++ "," ++ (val a2) 
                                        ++ "," ++ (val a3) 
                                        ++ ")"
                ScaleX a -> "scaleX(" ++ val a ++ ")"
                ScaleY a -> "scaleY(" ++ val a ++ ")"
                ScaleZ a -> "scaleZ(" ++ val a ++ ")"
                Rotate unit a -> "rotate(" ++ rval unit a ++ ")"
                Rotate3d unit a1 a2 a3 a4 -> 
                                          "scale3d(" ++ (val a1) 
                                              ++ "," ++ (val a2) 
                                              ++ "," ++ (val a3) 
                                              ++ "," ++ (rval unit a4) 
                                              ++ ")"

                RotateX unit a -> "rotateX(" ++ rval unit a ++ ")"
                RotateY unit a -> "rotateY(" ++rval unit a ++ ")"
                Skew unit a1 a2 -> 
                              "skew(" ++ (rval unit a1) 
                               ++ "," ++ (rval unit a2) 
                               ++ ")"
                SkewX unit a -> "skewX(" ++ rval unit a ++ ")"
                SkewY unit a -> "skewY(" ++ rval unit a ++ ")"
                Perspective a -> "perspective(" ++ (val a) ++ ")"


isDone : Time -> PropAnimation -> Bool
isDone elapsed propAnim = 
              let
                done a = A.isDone elapsed a
              in 
                case propAnim of
                    Prop _ _ a -> done a
                    Opacity a -> done a
                    Height _ a -> done a
                    Width _ a -> done a
                    Left _ a -> done a
                    Right _ a -> done a
                    Bottom _ a -> done a
                    Top _ a -> done a
                    Translate _ a1 a2 -> done a1 && done a2
                    Translate3d _ a1 a2 a3 -> done a1 && done a2 && done a3
                    TranslateX _ a -> done a
                    TranslateY _ a -> done a
                    Scale a1 -> done a1 
                    Scale3d a1 a2 a3 -> done a1 && done a2 && done a3
                    ScaleX a -> done a
                    ScaleY a -> done a
                    ScaleZ a -> done a
                    Rotate _ a -> done a
                    Rotate3d _ a1 a2 a3 a4 -> done a1 && done a2 && done a3 && done a4
                    RotateX _ a -> done a
                    RotateY _ a -> done a
                    Skew _ a1 a2 -> done a1 && done a2 
                    SkewX _ a -> done a
                    SkewY _ a -> done a
                    Perspective a -> done a


dUnit : DistanceUnit -> String -> String
dUnit unit num = 
          case unit of
            Px -> num ++ "px"

            Percent -> num ++ "%"

            Rem -> num ++ "rem"

            Em -> num ++ "em"


rUnit : RotationUnit -> String -> String
rUnit unit num =
          case unit of
            Deg -> num ++ "deg"

            Grad -> num ++ "grad"

            Rad -> num ++ "rad"

            Turn -> num ++ "turn"


animate : List PropAnimation -> Model -> ( Model, Effects Action )
animate anims model = update (Begin anims) model


update : Action -> Model -> ( Model, Effects Action )
update action model = 
        case action of

          Begin anim ->
            ( { model | anim = Just anim
                      , elapsed = 0.0
                      , start = Nothing }
            , Effects.tick Tick )

          Tick now ->
            let
              start = 
                case model.start of
                  Nothing -> now
                  Just t -> t
              newElapsed = now - start

              done = 
                case model.anim of
                  Nothing -> True
                  Just anims ->
                    List.all 
                      (isDone newElapsed) anims

            in
              if done then
                ( { model | elapsed = newElapsed }
                , Effects.none )
              else
                ( { model | elapsed = newElapsed 
                          , start = Just start }
                , Effects.tick Tick )



fadeIn : Time -> PropAnimation
fadeIn dur = Opacity 
               (A.animation 0 
                    |> A.from 0.0 
                    |> A.to 1.0
                    |> A.duration dur) 

fadeOut : Time -> PropAnimation
fadeOut dur = Opacity 
                 (A.animation 0 
                      |> A.from 1.0 
                      |> A.to 0.0
                      |> A.duration dur) 








