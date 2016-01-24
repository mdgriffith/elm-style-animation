module Html.Animation 
    ( StyleAnimation
    , StyleAction
    , StyleProperty (..)
    , Length (..), Angle (..) 
    , ColorFormat (..), ColorAlphaFormat (..)
    , initStyle
    , updateStyle
    , render
    , animate, animateOn
    , queue, queueOn
    , props, duration, easing
    , andThen, forwardTo
    , to, add, minus
    , (:=), (+=), (-=)
    ) where

{-| This library is for animating css properties (and works well with elm-html).

# Definition
@docs StyleAnimation, StyleAction

# All Animatable Style Properties and their Units
@docs StyleProperty, Length, Angle, ColorFormat, ColorAlphaFormat

# Interrupting an animation with a new one
@docs animate, animateOn

# Queueing an Animation
@docs queue, queueOn

# Creating a starting style
@docs initStyle

# Creating an animation
@docs props, duration, easing

# Animating Properties
@docs to, add, minus, (:=), (+=), (-=)

# Chaining animations together
@docs andThen

# Render a StyleAnimation into css you can use
@docs render, update

# Managing a list of styled widgets
@docs forwardTo

-}


import Effects exposing (Effects)
import Time exposing (Time, second)
import String exposing (concat)
import List 


type alias Model =
            { start : Maybe Time
            , elapsed : Time
            , anim : List StyleKeyframe
            , previous : Style
            }

{-| An Animation of CSS properties.
-}
type StyleAnimation = A Model


type alias Static = Float


type alias Dynamic 
         = (Float -> Float -> Float)

{-| Represent a CSS style.
This is done as a list of style properties with concrete/static values.
-}
type alias Style 
         = List (StyleProperty Static)

{-| Represent a style animation.
This is a list of StylePropertys, but instead of having a static value like '5', 
it has a function that takes the previous value, the current time, and provides the current value.
-}
type alias StyleKeyframe =
            { target : List (StyleProperty Dynamic)
            , duration : Time
            , ease : (Float -> Float)
            }

{-| All currently animatable properties.
-}
type StyleProperty a
        = Prop String a String
        | Opacity a
        | Height a Length
        | Width a Length
        | Left a Length
        | Top a Length
        | Right a Length
        | Bottom a Length

        | Padding a Length
        | PaddingLeft a Length
        | PaddingRight a Length
        | PaddingTop a Length
        | PaddingBottom a Length

        | Margin a Length
        | MarginLeft a Length
        | MarginRight a Length
        | MarginTop a Length
        | MarginBottom a Length

        -- Color
        | Color ColorFormat a a a
        | BackgroundColor ColorFormat a a a

        | ColorA ColorAlphaFormat a a a a
        | BackgroundColorA ColorAlphaFormat a a a a

        -- Transformations
        | Matrix a a a a a a 
        | Matrix3d a a a a a a a a a a a a a a a a 
        | Translate a a Length
        | Translate3d a a a Length
        | TranslateX a Length
        | TranslateY a Length
        | Scale a
        | Scale3d a a a
        | ScaleX a
        | ScaleY a
        | ScaleZ a
        | Rotate a Angle
        | Rotate3d a a a a Angle
        | RotateX a Angle
        | RotateY a Angle
        | Skew a a Angle
        | SkewX a Angle 
        | SkewY a Angle
        | Perspective a


{-| Units representing length.
-}
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

{-| Units representing angles.
-}
type Angle
      = Deg
      | Grad
      | Rad
      | Turn

{-| Units representing color.  Hex codes aren't currently supported, but may be in the future if they're wanted.
-}
type ColorFormat
      = RGB
      | HSL


{-| Units representing color that has an alpha channel.
-}
type ColorAlphaFormat
        = RGBA
        | HSLA


{-| StyleActions to be run on an animation. 
Queue will add a list of animations to the queue.
Interrupt will stop all animations and start the one that is provided.
-}
type StyleAction 
        = Queue (List StyleKeyframe)
        | Interrupt (List StyleKeyframe)
        | Tick Time

-- private
empty : Model
empty = { elapsed = 0.0
        , start = Nothing
        , anim = []
        , previous = []
        }

-- private
emptyKeyframe : StyleKeyframe
emptyKeyframe =
                 { target = []
                 , duration = defaultDuration
                 , ease = defaultEasing 
                 }

{-| Create an initial style state
-}
initStyle : Style -> StyleAnimation
initStyle sty = A { empty | previous = sty }


-- private
defaultDuration : Float
defaultDuration = 0.4 * second

-- private
defaultEasing : Float -> Float
defaultEasing x = (1 - cos (pi*x))/2



{-| Update an animation.
-}
updateStyle : StyleAction -> StyleAnimation -> ( StyleAnimation, Effects StyleAction )
updateStyle action (A model) =
       
        case action of

          Queue anims ->
                ( A { model | anim = model.anim ++ anims }
                , Effects.tick Tick )


          Interrupt anims ->
             let
                currentAnim = List.head model.anim
                previous = 
                  case currentAnim of
                    Nothing -> model.previous
                    Just a -> 
                      bake model.elapsed a model.previous
              in
                ( A { model | anim = anims
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

              currentAnim = List.head model.anim
              remaining = List.tail model.anim
            in
              case currentAnim of
                Nothing ->
                   ( A { model | elapsed = 0.0 
                               , start = Nothing
                               , previous = model.previous
                               , anim = model.anim }
                   , Effects.none )

                Just current ->
                  if newElapsed >= current.duration then
                    let
                      anims = 
                        case remaining of
                          Nothing -> []
                          Just a -> a

                      previous = 
                          bake current.duration current model.previous

                      resetElapsed = 
                          newElapsed - current.duration
                              
                    in
                      ( A { model | elapsed = resetElapsed
                                  , start = Just (now - resetElapsed)
                                  , previous = previous
                                  , anim = anims }
                      , Effects.tick Tick )

                  else
                     ( A { model | elapsed = newElapsed 
                                 , start = Just start
                                 }
                     , Effects.tick Tick )




{-| Syntactic sugar for running an Interrupt update.
-}
animate : List StyleKeyframe -> StyleAnimation -> ( StyleAnimation, Effects StyleAction )
animate anims model = updateStyle (Interrupt anims) model


{-| Same as animate, except with the arguments flipped.  This is useful only animating only a single style.

     UI.animateOn model.menuStyle
         <| UI.duration (0.4*second)
         <| UI.props 
             [ UI.Left UI.Px (UI.to 0) 
             , UI.Opacity (UI.to 1)
             ] [] 
instead of 

      UI.animate 
         (  UI.duration (0.4*second)
         <| UI.props 
             [ UI.Left UI.Px (UI.to 0) 
             , UI.Opacity (UI.to 1)
             ] [] 
         ) 
        model.menuStyle

-}
animateOn : StyleAnimation -> List StyleKeyframe -> ( StyleAnimation, Effects StyleAction )
animateOn model anims = animate anims model


{-| Syntactic sugar for running a Queue update.
-}
queue : List StyleKeyframe -> StyleAnimation -> ( StyleAnimation, Effects StyleAction )
queue anims model = updateStyle (Queue anims) model


{-| Same as queue, except with the arguments flipped.  This is useful when only animating only a single style. See animateOn for an example.
-}
queueOn : StyleAnimation -> List StyleKeyframe -> ( StyleAnimation, Effects StyleAction )
queueOn model anims = animate anims model


{-| Specify the properties that should be animated

     UI.animateOn model.menuStyle
                 <| UI.duration (0.4*second)
                 <| UI.props 
                     [ UI.Left UI.Px (UI.to 0) 
                     , UI.Opacity (UI.to 1)
                     ] [] -- every animation has to be 'started' with an empty list

-}
props : List (StyleProperty Dynamic) -> List StyleKeyframe -> List StyleKeyframe
props p anim = updateOrCreate anim (\anim -> { anim | target = p})


{-| Specify a duration for a keyframe.  If a duration isn't specified, the default is 400ms.
-}
duration : Time -> List StyleKeyframe -> List StyleKeyframe
duration dur anim = updateOrCreate anim (\anim -> { anim | duration = dur })
      

{-| Specify an easing function for a keyframe.  It is expected that values should start on 0 and end at 1.  The default is a sinusoidal
in-out.
-}
easing : (Float -> Float) -> List StyleKeyframe -> List StyleKeyframe
easing ease anim = updateOrCreate anim (\anim -> { anim | ease = ease })


{-| Append another keyframe.
-}
andThen : List StyleKeyframe -> List StyleKeyframe
andThen x = emptyKeyframe :: x

-- private
updateOrCreate : List StyleKeyframe -> (StyleKeyframe -> StyleKeyframe) -> List StyleKeyframe
updateOrCreate styles fn =
                 case styles of
                    [] -> [fn emptyKeyframe] 
                    cur::rem -> (fn cur)::rem


{-| Convenient function to forward an update to a style object contained in a type
 See the Showcase Example to get an idea of whats going on here
-}
forwardTo : (a -> StyleAnimation) -> (a -> StyleAnimation -> a) -> Int -> List a -> (StyleAnimation -> (StyleAnimation, (Effects StyleAction))) -> (List a, Effects StyleAction)
forwardTo styleGet styleSet i widgets fn = 
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



{-| Used for animating a StyleProperty to a value
Takes 
     * provided value
     * previous value
     * current normalized time (0.0-1.0) 
     * returns current value

-}
to : Float -> Float -> Float -> Float
to target from current = ((target-from) * current) + from

{-| Used for animating a StyleProperty by adding to its existing value

-}
add : Float -> Float -> Float -> Float
add mod from current = 
        let
          target = from + mod
        in
          to target from current

{-| Used for animating a StyleProperty by subtracting from its existing value

-}
minus : Float -> Float -> Float -> Float
minus mod from current = 
        let
          target = from - mod
        in
          to target from current

{-| Infix version of the above `to` function

-}
(:=) : Float -> Float -> Float -> Float
(:=) t f c = to t f c

{-| Infix version of the above `add` function

-}
(+=) : Float -> Float -> Float -> Float
(+=) t f c = add t f c

{-| Infix version of the above `minus` function

-}
(-=) : Float -> Float -> Float -> Float
(-=) t f c = minus t f c




-- private
findProp : Style -> StyleProperty a -> Maybe (StyleProperty Static)
findProp state prop =
            let
              findBy fn xs = List.head (List.filter fn xs)
              matchPropID a b = propId a == propId b
            in 
              findBy (matchPropID prop) state

{-| Render into concrete css that can be directly applied to 'style' in elm-html

    div [ style (UI.render widget.style) ] [ ]

-}
render : StyleAnimation -> List (String, String)
render (A model) = 
        let
          currentAnim = List.head model.anim
        in
          case currentAnim of
            Nothing -> 
              let
                rendered = 
                    List.map renderProp model.previous

                transformsNprops = 
                    List.partition (\(name,_) -> name == "transform") rendered

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
                 

-- private
renderProp : StyleProperty Static -> (String, String)
renderProp prop = ( renderName prop 
                  , renderValue prop
                  )

-- private
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

-- private
fill : List (StyleProperty Static) -> List (StyleProperty Static) -> List (StyleProperty Static)
fill new existing =
           List.foldl
                    (\x acc ->
                      case findProp acc x of
                        Nothing -> x::acc 
                        Just _ -> acc
                    ) new existing

-- private
-- Converts an animation into a Style that can be rendered.
bake : Time -> StyleKeyframe -> Style -> Style
bake elapsed anim prev = 
          let
            percentComplete = 
                 elapsed / anim.duration

            eased = 
                 anim.ease percentComplete

            style = 
                List.map 
                  (\p -> bakeProp p (findProp prev p) eased) 
                    anim.target
          in
            -- If properties are in previous
            -- but not in the current animation
            -- copy them over as is
            fill style prev

-- private
bakeProp : StyleProperty Dynamic -> Maybe (StyleProperty Static) -> Float -> StyleProperty Static
bakeProp prop prev current =
            let
              val from fn = fn from current
            in
              case prop of
                Prop name to unit -> 
                  let
                    from =
                      case prev of
                        Just (Prop _ x _) -> x
                        _ -> 0.0
                  in
                    Prop name (val from to) unit


                Opacity to -> 
                  let
                    from =
                      case prev of
                        Just (Opacity x) -> x
                        _ -> 0.0
                  in
                    Opacity (val from to)


                Height to unit -> 
                  let
                    from =
                      case prev of
                        Just (Height x _) -> x
                        _ -> 0.0
                  in
                    Height (val from to) unit


                Width to unit -> 
                  let
                    from =
                      case prev of
                        Just (Width x _) -> x
                        _ -> 0.0
                  in
                    Width (val from to) unit


                Left to unit -> 
                  let
                    from =
                      case prev of
                        Just (Left x _) -> x
                        _ -> 0.0
                  in
                    Left (val from to) unit


                Top to unit -> 
                  let
                    from =
                      case prev of
                        Just (Top x _) -> x
                        _ -> 0.0
                  in
                    Top (val from to) unit


                Right to unit ->  
                  let
                    from =
                      case prev of
                        Just (Right x _) -> x
                        _ -> 0.0
                  in
                    Right (val from to) unit


                Bottom to unit -> 
                  let
                    from =
                      case prev of
                        Just (Bottom x _) -> x
                        _ -> 0.0
                  in
                    Bottom (val from to) unit


                Padding to unit -> 
                  let
                    from =
                      case prev of
                        Just (Padding x _) -> x
                        _ -> 0.0
                  in
                    Padding (val from to) unit


                PaddingLeft to unit -> 
                  let
                    from =
                      case prev of
                        Just (PaddingLeft x _) -> x
                        _ -> 0.0
                  in
                    PaddingLeft (val from to) unit


                PaddingRight to unit  -> 
                  let
                    from =
                      case prev of
                        Just (PaddingRight x _) -> x
                        _ -> 0.0
                  in
                    PaddingRight (val from to) unit


                PaddingTop to unit -> 
                  let
                    from =
                      case prev of
                        Just (PaddingTop x _) -> x
                        _ -> 0.0
                  in
                    PaddingTop (val from to) unit


                PaddingBottom to unit -> 
                  let
                    from =
                      case prev of
                        Just (PaddingBottom x _) -> x
                        _ -> 0.0
                  in
                    PaddingBottom (val from to) unit


                Margin to unit ->
                  let
                    from =
                      case prev of
                        Just (Margin x _) -> x
                        _ -> 0.0
                  in
                    Margin (val from to) unit


                MarginLeft to unit   -> 
                  let
                    from =
                      case prev of
                        Just (MarginLeft x _) -> x
                        _ -> 0.0
                  in
                    MarginLeft (val from to) unit


                MarginRight to unit  -> 
                  let
                    from =
                      case prev of
                        Just (MarginRight x _) -> x
                        _ -> 0.0
                  in
                    MarginRight (val from to) unit


                MarginTop to unit -> 
                  let
                    from =
                      case prev of
                        Just (MarginTop x _) -> x
                        _ -> 0.0
                  in
                    MarginTop (val from to) unit 


                MarginBottom to unit ->  
                  let
                    from =
                      case prev of
                        Just (MarginBottom x _) -> x
                        _ -> 0.0
                  in
                    MarginBottom (val from to) unit


                Color unit x y z    -> 
                  let
                    (xFrom, yFrom, zFrom) =
                      case prev of
                        Just (Color _ x1 y1 z1) -> (x1, y1, z1)
                        _ -> (0.0, 0.0, 0.0)
                  in
                    Color unit (val xFrom x) (val yFrom y) (val zFrom z)


                BackgroundColor unit x y z -> 
                  let
                    (xFrom, yFrom, zFrom) =
                      case prev of
                        Just (BackgroundColor _ x1 y1 z1) -> (x1, y1, z1)
                        _ -> (0.0, 0.0, 0.0)
                  in
                    BackgroundColor unit (val xFrom x) (val yFrom y) (val zFrom z)


                ColorA unit x y z a -> 
                  let
                    (xFrom, yFrom, zFrom, aFrom) =
                      case prev of
                        Just (ColorA _ x1 y1 z1 a1) -> (x1, y1, z1, a1)
                        _ -> (0.0, 0.0, 0.0, 0.0)
                  in
                    ColorA unit (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a)


                BackgroundColorA unit x y z a -> 
                  let
                    (xFrom, yFrom, zFrom, aFrom) =
                      case prev of
                        Just (BackgroundColorA _ x1 y1 z1 a1) -> (x1, y1, z1, a1)
                        _ -> (0.0, 0.0, 0.0, 0.0)
                  in
                    BackgroundColorA unit (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a)



                Translate x y unit -> 
                  let
                    (xFrom, yFrom) =
                      case prev of
                        Just (Translate x1 y1 _) -> (x1, y1)
                        _ -> (0.0, 0.0)
                  in
                    Translate (val xFrom x) (val yFrom y) unit
                    

                Translate3d x y z unit -> 
                  let
                    (xFrom, yFrom, zFrom) =
                      case prev of
                        Just (Translate3d x1 y1 z1 _) -> (x1, y1, z1)
                        _ -> (0.0, 0.0, 0.0)
                  in
                    Translate3d (val xFrom x) (val yFrom y) (val zFrom z) unit
                  

                TranslateX to unit -> 
                  let
                    from =
                      case prev of
                        Just (TranslateX x _) -> x
                        _ -> 0.0
                  in
                    TranslateX (val from to) unit


                TranslateY to unit -> 
                  let
                    from =
                      case prev of
                        Just (TranslateY x _) -> x
                        _ -> 0.0
                  in
                    TranslateY (val from to) unit


                Scale to -> 
                  let
                    from =
                      case prev of
                        Just (Scale x) -> x
                        _ -> 0.0
                  in
                     Scale (val from to)


                Scale3d x y z -> 
                  let
                    (xFrom, yFrom, zFrom) =
                      case prev of
                        Just (Scale3d x1 y1 z1) -> (x1, y1, z1)
                        _ -> (0.0, 0.0, 0.0)
                  in
                    Scale3d (val xFrom x) (val yFrom y) (val zFrom z)
                   

                ScaleX to -> 
                  let
                    from =
                      case prev of
                        Just (ScaleX x) -> x
                        _ -> 0.0
                  in
                    ScaleX (val from to)


                ScaleY to -> 
                  let
                    from =
                      case prev of
                        Just (ScaleY x) -> x
                        _ -> 0.0
                  in
                    ScaleY (val from to)


                ScaleZ to -> 
                  let
                    from =
                      case prev of
                        Just (ScaleZ x) -> x
                        _ -> 0.0
                  in
                    ScaleZ (val from to)


                Rotate to unit -> 
                  let
                    from =
                      case prev of
                        Just (Rotate x _) -> x
                        _ -> 0.0
                  in
                    Rotate (val from to) unit


                Rotate3d x y z a unit ->
                  let
                    (xFrom, yFrom, zFrom, aFrom) =
                      case prev of
                        Just (Rotate3d x1 y1 z1 a1 _) -> (x1, y1, z1, a1)
                        _ -> (0.0, 0.0, 0.0, 0.0)
                  in 
                    Rotate3d (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a) unit
                   

                RotateX to unit -> 
                  let
                    from =
                      case prev of
                        Just (RotateX x _) -> x
                        _ -> 0.0
                  in
                    RotateX (val from to) unit

                RotateY to unit -> 
                  let
                    from =
                      case prev of
                        Just (RotateY x _) -> x
                        _ -> 0.0
                  in
                    RotateY (val from to) unit


                Skew x y unit ->
                  let
                    (xFrom, yFrom) =
                      case prev of
                        Just (Skew x y _) -> (x, y)
                        _ -> (0.0, 0.0)
                  in
                    Skew (val xFrom x) (val yFrom y) unit 
                    

                SkewX to unit -> 
                  let
                     from =
                      case prev of
                        Just (SkewX x _) -> x
                        _ -> 0.0
                  in
                    SkewX (val from to) unit

                SkewY to unit -> 
                  let
                     from =
                      case prev of
                        Just (SkewY x _) -> x
                        _ -> 0.0
                  in
                    SkewY (val from to) unit

                Perspective to -> 
                  let
                     from =
                      case prev of
                        Just (SkewY x _) -> x
                        _ -> 0.0
                  in
                    Perspective (val from to)

                Matrix a b c x y z -> 
                  let
                    (aFrom, bFrom, cFrom, xFrom, yFrom, zFrom) =
                      case prev of
                        Just (Matrix a2 b2 c2 x2 y2 z2) -> 
                            (a2, b2, c2, x2, y2, z2)
                        _ -> 
                            (0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
                  in 
                    Matrix (val aFrom a) (val bFrom b) (val cFrom c) (val xFrom x) (val yFrom y) (val zFrom z)
                


                Matrix3d a b c d e f g h i j k l m n o p -> 
                      case prev of

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

-- private
-- renders a valid css value for a Style Property
renderValue : StyleProperty Static -> String
renderValue prop  =
            let
              val a = toString a
              renderLength a unit = (val a) ++ lenUnit unit
              renderAngle a unit =  (val a) ++ angleUnit unit
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
                Prop _ a u -> (val a) ++ u

                Opacity a -> val a
                Height a unit -> renderLength a unit
                Width a unit -> renderLength a unit
                Left a unit -> renderLength a unit
                Top a unit -> renderLength a unit
                Right a unit -> renderLength a unit
                Bottom a unit -> renderLength a unit

                Padding a unit       -> renderLength a unit 
                PaddingLeft a unit   -> renderLength a unit 
                PaddingRight a unit  -> renderLength a unit
                PaddingTop a unit    -> renderLength a unit 
                PaddingBottom a unit -> renderLength a unit 

                Margin a unit       -> renderLength a unit 
                MarginLeft a unit   -> renderLength a unit 
                MarginRight a unit  -> renderLength a unit 
                MarginTop a unit    -> renderLength a unit 
                MarginBottom a unit -> renderLength a unit 

                Color unit x y z    -> 
                      (colorUnit unit) ++ renderIntList [x,y,z]

                BackgroundColor unit x y z -> 
                       (colorUnit unit) ++ renderIntList [x,y,z]

                ColorA unit x y z a -> 
                      (colorAUnit unit) ++ renderColorA (x,y,z,a)

                BackgroundColorA unit x y z a -> 
                      (colorAUnit unit) ++ renderColorA (x,y,z,a)


                Translate a1 a2 unit -> 
                        "translate(" ++ (renderLength a1 unit) 
                              ++ "," ++ (renderLength a2 unit) 
                              ++ ")"

                Translate3d a1 a2 a3 unit -> 
                          "translate3d(" ++ (renderLength a1 unit) 
                                 ++ "," ++ (renderLength a2 unit) 
                                 ++  "," ++ (renderLength a3 unit) 
                                 ++ ")"

                TranslateX a unit -> "translateX(" ++ renderLength a unit ++ ")"
                TranslateY a unit -> "translateY(" ++ renderLength a unit ++ ")"
                Scale a1 -> "scale(" ++ (val a1)  ++ ")"
                Scale3d a1 a2 a3 -> "scale3d(" ++ (val a1) 
                                        ++ "," ++ (val a2) 
                                        ++ "," ++ (val a3) 
                                        ++ ")"
                ScaleX a -> "scaleX(" ++ val a ++ ")"
                ScaleY a -> "scaleY(" ++ val a ++ ")"
                ScaleZ a -> "scaleZ(" ++ val a ++ ")"
                Rotate a unit -> "rotate(" ++ renderAngle a unit ++ ")"
                Rotate3d a1 a2 a3 a4 unit -> 
                                          "rotate3d(" ++ (val a1) 
                                              ++ "," ++ (val a2) 
                                              ++ "," ++ (val a3) 
                                              ++ "," ++ (renderAngle a4 unit) 
                                              ++ ")"

                RotateX a unit -> "rotateX(" ++ renderAngle a unit ++ ")"
                RotateY a unit -> "rotateY(" ++renderAngle a unit ++ ")"
                Skew a1 a2 unit -> 
                              "skew(" ++ (renderAngle a1 unit) 
                               ++ "," ++ (renderAngle a2 unit) 
                               ++ ")"
                SkewX a unit -> "skewX(" ++ renderAngle a unit ++ ")"
                SkewY a unit -> "skewY(" ++ renderAngle a unit ++ ")"
                Perspective a -> "perspective(" ++ (val a) ++ ")"

                Matrix a b c x y z -> 
                        "matrix" ++ 
                          (renderList [a,b,c,x,y,z])
                        
                
                Matrix3d a b c d e f g h i j k l m n o p -> 
                        "matrix3d" ++ 
                          (renderList [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p])
                           



-- private
propId : StyleProperty a -> String
propId prop =
        case prop of
          Prop name _ unit -> name ++ unit
          Opacity _ -> "opacity"
          Height _ unit -> "height" ++ (lenUnit unit)
          Width _ unit  -> "width" ++ (lenUnit unit)
          Left _ unit   -> "left" ++ (lenUnit unit)
          Right _ unit  -> "right" ++ (lenUnit unit)
          Bottom _ unit -> "bottom" ++ (lenUnit unit)
          Top _ unit    -> "top" ++ (lenUnit unit)

          Padding _ unit      -> "padding" ++ (lenUnit unit)
          PaddingLeft _ unit  -> "padding-left" ++ (lenUnit unit)
          PaddingRight _ unit -> "padding-right" ++ (lenUnit unit)
          PaddingTop _ unit   -> "padding-top" ++ (lenUnit unit)
          PaddingBottom _ unit-> "padding-bottom" ++ (lenUnit unit)

          Margin _ unit      -> "margin" ++ (lenUnit unit)
          MarginLeft _ unit  -> "margin-left" ++ (lenUnit unit)
          MarginRight _ unit -> "margin-right" ++ (lenUnit unit)
          MarginTop _ unit   -> "margin-top" ++ (lenUnit unit)
          MarginBottom _ unit-> "margin-bottom" ++ (lenUnit unit)

          Color unit _ _ _    -> "color" ++ (colorUnit unit)
          BackgroundColor unit _ _ _ -> "background-color" ++ (colorUnit unit)

          ColorA unit _ _ _ _ -> "color" ++ (colorAUnit unit)
          BackgroundColorA unit _ _ _ _ -> "background-color" ++ (colorAUnit unit)

          Matrix _ _ _ _ _ _ -> "matrix"
          Matrix3d _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ -> "matrix3d"
          Translate _ _ unit -> "translate" ++ (lenUnit unit)
          Translate3d _ _ _ unit -> "translate3d"  ++ (lenUnit unit)
          TranslateX _ unit -> "translatex" ++ (lenUnit unit)
          TranslateY _ unit -> "translatey" ++ (lenUnit unit)
          Scale _        -> "scale"
          Scale3d _ _ _  -> "scale3d"
          ScaleX _   -> "scalex"
          ScaleY _   -> "scaley"
          ScaleZ _   -> "scalez"
          Rotate _ unit -> "rotate" ++ (angleUnit unit)
          Rotate3d _ _ _ _ unit -> "rotate3d" ++ (angleUnit unit)
          RotateX _ unit   -> "rotatex" ++ (angleUnit unit)
          RotateY _ unit   -> "rotatey" ++ (angleUnit unit)
          Skew _ _ unit   -> "skew" ++ (angleUnit unit)
          SkewX _ unit     -> "skewx" ++ (angleUnit unit)
          SkewY _ unit     -> "skewy" ++ (angleUnit unit)
          Perspective _ -> "perspective"



-- private
colorUnit : ColorFormat -> String
colorUnit color =
            case color of
              RGB -> "rgb"
              HSL -> "hsl"

-- private
colorAUnit : ColorAlphaFormat -> String
colorAUnit color =
            case color of
              RGBA -> "rgba"
              HSLA -> "hsla"

-- private
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

-- private
angleUnit : Angle -> String
angleUnit unit = 
          case unit of
            Deg -> "deg"
            Grad -> "grad"
            Rad -> "rad"
            Turn -> "turn"








