module ElmUI where


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, second)
import Effects exposing (Effects)

import Signal exposing (Address)

import Animation as A

import List


type alias Model = { start : Maybe Time 
                   , elapsed : Time
                   , anim : Maybe (List PropAnimation)
                   }


type PropAnimation
        = Opacity A.Animation
        | Height DistanceUnit A.Animation
        | Width DistanceUnit A.Animation
        | Left DistanceUnit A.Animation
        | Top DistanceUnit A.Animation
        | Right DistanceUnit A.Animation
        | Bottom DistanceUnit A.Animation
        | Prop String String A.Animation

type Transform 
        = NoTransform
        --| Matrix --Float Float Float Float Float Float 
        --| Matrix3d -- Float Float Float Float Float Float Float Float Float Float Float Float Float Float Float Float 
        | Translate DistanceUnit A.Animation A.Animation
        | Translate3d DistanceUnit A.Animation A.Animation A.Animation
        | TranslateX DistanceUnit A.Animation
        | TranslateY DistanceUnit A.Animation
        | Scale A.Animation A.Animation
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
        | SkeyY RotationUnit A.Animation
        | Perspective A.Animation
        | Initial
        | Inherit


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





--none  Defines that there should be no transformation  Play it »
--matrix(n,n,n,n,n,n) Defines a 2D transformation, using a matrix of six values Play it »
--matrix3d(n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n) Defines a 3D transformation, using a 4x4 matrix of 16 values  
--translate(x,y)  Defines a 2D translation  Play it »
--translate3d(x,y,z)  Defines a 3D translation  
--translateX(x) Defines a translation, using only the value for the X-axis  Play it »
--translateY(y) Defines a translation, using only the value for the Y-axis  Play it »
--translateZ(z) Defines a 3D translation, using only the value for the Z-axis 
--scale(x,y)  Defines a 2D scale transformation Play it »
--scale3d(x,y,z)  Defines a 3D scale transformation 
--scaleX(x) Defines a scale transformation by giving a value for the X-axis Play it »
--scaleY(y) Defines a scale transformation by giving a value for the Y-axis Play it »
--scaleZ(z) Defines a 3D scale transformation by giving a value for the Z-axis  
--rotate(angle) Defines a 2D rotation, the angle is specified in the parameter  Play it »
--rotate3d(x,y,z,angle) Defines a 3D rotation 
--rotateX(angle)  Defines a 3D rotation along the X-axis  Play it »
--rotateY(angle)  Defines a 3D rotation along the Y-axis  Play it »
--rotateZ(angle)  Defines a 3D rotation along the Z-axis  Play it »
--skew(x-angle,y-angle) Defines a 2D skew transformation along the X- and the Y-axis  Play it »
--skewX(angle)  Defines a 2D skew transformation along the X-axis Play it »
--skewY(angle)  Defines a 2D skew transformation along the Y-axis Play it »
--perspective(n)  Defines a perspective view for a 3D transformed element 
--initial Sets this property to its default value. Read about initial 
--inherit Inherits this property from its parent element. Read about inherit



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
                List.map (renderSplat model.elapsed) anim
                

renderSplat : Time -> PropAnimation -> (String, String)
renderSplat elapsed propAnim =
                let
                  (propName, anim) = 
                    case propAnim of
                      Prop str _ a -> (str, a)
                      Opacity a -> ("opacity", a)
                      Height _ a -> ("height", a)
                      Width _ a -> ("width", a)
                      Left _ a -> ("left", a)
                      Right _ a -> ("right", a)
                      Bottom _ a -> ("bottom", a)
                      Top _ a -> ("top", a)
 
                  propValue = renderUnit propAnim (A.animate elapsed anim)
                in
                  (propName, propValue)

renderUnit : PropAnimation -> Float -> String
renderUnit prop val =
              case prop of
                Opacity _ -> toString val
                Height unit _ -> renderDistanceUnit unit val
                Width unit _ -> renderDistanceUnit unit val
                Left unit _ -> renderDistanceUnit unit val
                Top unit _ -> renderDistanceUnit unit val
                Right unit _ -> renderDistanceUnit unit val
                Bottom unit _ -> renderDistanceUnit unit val
                Prop _ u _ -> toString val ++ u


renderDistanceUnit : DistanceUnit -> Float -> String
renderDistanceUnit unit num = 
                  case unit of
                    Px -> toString num ++ "px"

                    Percent -> toString num ++ "%"

                    Rem -> toString num ++ "rem"

                    Em -> toString num ++ "em"





animate anims model = update (Begin anims) model


--propStartValues : List PropAnimation -> List (PropAnimation, Float)
--propStartValues propAnims = 
--                  List.map (\pa -> (pa, A.getFrom pa.animation)) propAnims


--propEndValues : List PropAnimation -> List (PropAnimation, Float)
--propEndValues propAnims = 
--                  List.map (\pa -> (pa, A.getTo pa.animation)) propAnims

--equalAnimationValues : List (PropAnimation, Float) -> List (PropAnimation, Float) -> Bool
--equalAnimationValues props1 props2 = 
--                          List.all
--                            (\(p1, v1) ->
--                                List.any 
--                                  (\(p2, v2) ->
--                                    p1.property == p2.property
--                                    && v1 == v2
--                                  ) 
--                                props2
--                            )
--                            props1



update : Action -> Model -> ( Model, Effects Action )
update action model = 
        case action of

          Begin anim ->
            --let
            --  newEndValues = propEndValues anim
            --  alreadyGoingThere = 
            --    case model.anim of
            --      Nothing -> False
            --      Just a ->
            --        let
            --          oldEndValue = propEndValues a
            --        in
            --          equalAnimationValues newEndValues oldEndValue

             
            --in
            --  if alreadyGoingThere then
            --    ( model, Effects.none )
            --  else
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

--opacity : A.Animation -> PropAnimation
--opacity anim = 

--            { animation = anim
--               , property = Opacity 
--               }

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


--animate : Property -> A.Animation -> PropAnimation
--animate prop anim = { animation = anim
--                    , property = prop }




isDone : Time -> PropAnimation -> Bool
isDone elapsed propAnim = 
                  let
                    anim =
                      case propAnim of
                        Prop _ _ a -> a
                        Opacity a -> a
                        Height _ a -> a
                        Width _ a -> a
                        Left _ a -> a
                        Right _ a -> a
                        Bottom _ a -> a
                        Top _ a -> a
                  in 
                    A.isDone elapsed anim





