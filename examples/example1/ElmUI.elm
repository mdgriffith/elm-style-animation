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
                   , state : Maybe (List PropAnimation)
                   }


type PropAnimation
        = Prop String String A.Animation
        | Opacity A.Animation
        | Height Length A.Animation
        | Width Length A.Animation
        | Left Length A.Animation
        | Top Length A.Animation
        | Right Length A.Animation
        | Bottom Length A.Animation

        | Padding Length A.Animation
        | PaddingLeft Length A.Animation
        | PaddingRight Length A.Animation
        | PaddingTop Length A.Animation
        | PaddingBottom Length A.Animation

        | Margin Length A.Animation
        | MarginLeft Length A.Animation
        | MarginRight Length A.Animation
        | MarginTop Length A.Animation
        | MarginBottom Length A.Animation
        

        -- Transformations
--        --| Matrix --Float Float Float Float Float Float 
--        --| Matrix3d -- Float Float Float Float Float Float Float Float Float Float Float Float Float Float Float Float 
        | Translate Length A.Animation A.Animation
        | Translate3d Length A.Animation A.Animation A.Animation
        | TranslateX Length A.Animation
        | TranslateY Length A.Animation
        | Scale A.Animation
        | Scale3d A.Animation A.Animation A.Animation
        | ScaleX A.Animation
        | ScaleY A.Animation
        | ScaleZ A.Animation
        | Rotate Angle A.Animation
        | Rotate3d Angle A.Animation A.Animation A.Animation A.Animation
        | RotateX Angle A.Animation
        | RotateY Angle A.Animation
        | Skew Angle A.Animation A.Animation
        | SkewX Angle A.Animation 
        | SkewY Angle A.Animation
        | Perspective A.Animation

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



empty : Model
empty = { elapsed = 0.0
        , start = Nothing
        , anim = Nothing
        , state = Nothing
        }


type Action 
        = Begin (List PropAnimation)
        | Continue (List PropAnimation)
        | Tick Time


to : Float -> A.Animation
to t = A.animation 0.0 
                |> to t


animate : List PropAnimation -> Model -> ( Model, Effects Action )
animate anims model = update (Continue anims) model


zipMatching : List PropAnimation -> List PropAnimation -> List (Maybe PropAnimation, Maybe PropAnimation)
zipMatching anim1 anim2 = 
                  let
                    findBy fn xs = List.head (List.filter fn xs)
                    matchPropID a b = propId a == propId b

                    matching = 
                      List.map
                          (\a  ->
                            (Just a, findBy (matchPropID a)  anim2)
                          ) anim1

                    remaining = 
                      List.concatMap
                          (\a  ->
                            let
                              found = findBy (matchPropID a) anim1
                            in
                              case found of
                                Nothing ->
                                  [(Nothing, Just a)]
                                Just _ ->
                                  []
                          ) anim2
                  in
                    matching ++ remaining



--restart : Time -> A.Animation -> A.Animation
--restart start (A.A anim) =
--              A.A { anim | start = start } 


retarget : Time -> PropAnimation -> PropAnimation -> PropAnimation
retarget current anim1 anim2 = 
            let
              default = anim1
            in
              case anim1 of
                Prop name unit a -> 
                  case anim2 of
                    Prop _ _ a2 ->
                       Prop name unit (A.retarget 0 (A.getTo a2) a)
                    _ -> default

                Opacity a -> 
                  case anim2 of
                    Opacity a2 ->
                       Opacity (A.retarget current (A.getTo a2) a)
                    _ -> default

                Height unit a -> 
                  case anim2 of
                    Height _ a2 ->
                       Height unit (A.retarget 0 (A.getTo a2) a)
                    _ -> default

                Width unit a -> 
                  case anim2 of
                    Width _ a2 ->
                       Width unit (A.retarget 0 (A.getTo a2) a)
                    _ -> default

                Left unit a -> 
                  case anim2 of
                    Left _ a2 ->
                      Left unit (A.retarget current (A.getTo a2) a)
                    _ -> default

                Right unit a -> 
                  case anim2 of
                    Right _ a2 ->
                       Right unit (A.retarget 0 (A.getTo a2) a)
                    _ -> default

                Bottom unit a -> 
                  case anim2 of
                    Bottom _ a2 ->
                       Bottom unit (A.retarget 0 (A.getTo a2) a)
                    _ -> default

                Top unit a -> 
                  case anim2 of
                    Right _ a2 ->
                       Right unit (A.retarget 0 (A.getTo a2) a)
                    _ -> default

                Padding unit a       -> 
                  case anim2 of
                    Padding _ a2 ->
                       Padding unit (A.retarget 0 (A.getTo a2) a)
                    _ -> default

                PaddingLeft _ a   -> default 
                PaddingRight _ a  -> default
                PaddingTop _ a    -> default 
                PaddingBottom _ a -> default 

                Margin _ a       -> default 
                MarginLeft _ a   -> default 
                MarginRight _ a  -> default 
                MarginTop _ a    -> default 
                MarginBottom _ a -> default 

                Translate _ a1 a2 -> default
                Translate3d _ a1 a2 a3 -> default
                TranslateX _ a -> default
                TranslateY _ a -> default
                Scale a1 -> default
                Scale3d a1 a2 a3 -> default
                ScaleX a -> default
                ScaleY a -> default
                ScaleZ a -> default
                Rotate _ a -> default
                Rotate3d _ a1 a2 a3 a4 -> default
                RotateX _ a -> default
                RotateY _ a -> default
                Skew _ a1 a2 -> default 
                SkewX _ a -> default
                SkewY _ a -> default
                Perspective a -> default


resolveRetarget : Time -> (Maybe PropAnimation, Maybe PropAnimation) -> List PropAnimation
resolveRetarget current anims = 
                      case anims of
                        (Nothing, Nothing) -> []

                        (Just a, Nothing) -> [a]

                        (Nothing, Just b) -> [b]

                        (Just a, Just b) -> 
                          if isDone current a then
                            [b]
                          else
                            [retarget current a b]


resetElapsed : Time -> (Maybe PropAnimation, Maybe PropAnimation) -> Bool
resetElapsed current anims = 
                  case anims of
                    (Nothing, Nothing) -> True

                    (Just a, Nothing) -> True

                    (Nothing, Just b) -> True

                    (Just a, Just b) -> 
                      if isDone current a then
                        True
                      else
                        False

                          


update : Action -> Model -> ( Model, Effects Action )
update action model = 
        case action of

          Begin anims ->
            ( { model | anim = Just anims
                      , elapsed = 0.0
                      , start = Nothing }
            , Effects.tick Tick )

          Continue anims ->
            case model.anim of
              Nothing ->
                ( { model | anim = Just anims
                          , elapsed = 0.0
                          , start = Nothing }
                , Effects.tick Tick )

              Just modelAnim ->
                let
                  zipped = zipMatching modelAnim anims
                  resolved = List.concatMap (resolveRetarget model.elapsed) zipped
                  (start, elapsed) = 
                        if List.all (resetElapsed model.elapsed) zipped then
                          (Nothing, 0.0)
                        else
                          (model.start, model.elapsed)
                in
                  ( { model | anim = Just resolved 
                            , elapsed = elapsed
                            , start = start }
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
              renderLength unit a = addLengthUnits unit (val a)
              renderAngle unit a = addAngleUnits unit (val a)
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

                Translate unit a1 a2 -> "translate(" ++ (renderLength unit a1) 
                                              ++ "," ++ (renderLength unit a2) 
                                              ++ ")"
                Translate3d unit a1 a2 a3 -> "translate3d(" ++ (renderLength unit a1) 
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
                                          "scale3d(" ++ (val a1) 
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


isDone : Time -> PropAnimation -> Bool
isDone elapsed propAnim = 
              let
                done a = A.isDone elapsed a
              in 
                case propAnim of
                    Prop _ _ a  -> done a
                    Opacity a   -> done a
                    Height _ a  -> done a
                    Width _ a   -> done a
                    Left _ a    -> done a
                    Right _ a   -> done a
                    Bottom _ a  -> done a
                    Top _ a     -> done a

                    Padding _ a       -> done a 
                    PaddingLeft _ a   -> done a 
                    PaddingRight _ a  -> done a
                    PaddingTop _ a    -> done a 
                    PaddingBottom _ a -> done a 

                    Margin _ a       -> done a 
                    MarginLeft _ a   -> done a 
                    MarginRight _ a  -> done a 
                    MarginTop _ a    -> done a 
                    MarginBottom _ a -> done a 

                    Translate _ a1 a2      -> done a1 && done a2
                    Translate3d _ a1 a2 a3 -> done a1 && done a2 && done a3
                    TranslateX _ a -> done a
                    TranslateY _ a -> done a
                    Scale a1       -> done a1 
                    Scale3d a1 a2 a3 -> done a1 && done a2 && done a3
                    ScaleX a   -> done a
                    ScaleY a   -> done a
                    ScaleZ a   -> done a
                    Rotate _ a -> done a
                    Rotate3d _ a1 a2 a3 a4 -> done a1 && done a2 && done a3 && done a4
                    RotateX _ a -> done a
                    RotateY _ a -> done a
                    Skew _ a1 a2 -> done a1 && done a2 
                    SkewX _ a -> done a
                    SkewY _ a -> done a
                    Perspective a -> done a


propId : PropAnimation -> Int
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








