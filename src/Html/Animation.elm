module Html.Animation 
    ( StyleAnimation
    , StyleAction
    , initStyle
    , updateStyle
    , render
    , animate
    , queue
    , on
    , props, duration, easing
    , andThen, forwardTo
    , to, add, minus
    , (:=), (+=), (-=)
    , opacity
    , noUnit
    , px
    , left
    , distance
    , unitless
    ) where

{-| This library is for animating css properties (and works well with elm-html).

# Definition
@docs StyleAnimation, StyleAction

# All Animatable Style Properties and their Units
@docs opacity, px, noUnit, left, distance, unitless

# Interrupting an animation with a new one
@docs animate

# Queueing an Animation
@docs queue

# Updating an animation
@docs on

# Creating a starting style
@docs initStyle

# Creating an animation
@docs props, duration, easing

# Animating Properties
@docs to, add, minus, (:=), (+=), (-=)

# Chaining animations together
@docs andThen

# Render a StyleAnimation into css you can use
@docs render, updateStyle

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
         = List (StylePropWithUnit Static)

{-| Represent a style animation.
This is a list of StylePropertys, but instead of having a static value like '5', 
it has a function that takes the previous value, the current time, and provides the current value.
-}
type alias StyleKeyframe =
            { target : List (StyleProperty Dynamic)
            , duration : Time
            , ease : (Float -> Float)
            }



type StyleProperty a
        = R (AngleProperty a)
        | L (LengthProperty a)
        | C (Color a)
        | CA (ColorAlpha a)
        | U (Unitless a)

-- A style property with a unit
type StylePropWithUnit a 
        = Ru (Angle a)
        | Lu (Length a)
        | Cu (ColorFormat a)
        | CAu (ColorAlphaFormat a)
        | Uu (Unitless a)

removeUnits : StylePropWithUnit a -> StyleProperty a
removeUnits unitless = 
          case unitless of
            Ru a ->
              case a of
                Deg prop -> R prop
                Grad prop -> R prop
                Rad prop -> R prop
                Turn prop -> R prop
            Lu l ->
              case l of
                Px prop -> L prop
                Percent prop -> L prop
                Rem prop -> L prop
                Em prop -> L prop
                Ex prop -> L prop
                Ch prop -> L prop
                Vh prop -> L prop
                Vw prop -> L prop
                Vmin prop -> L prop
                Vmax prop -> L prop
                Mm prop -> L prop
                Cm prop -> L prop
                In prop -> L prop
                Pt prop -> L prop
                Pc prop -> L prop
            Cu c ->
              case c of
                RGB prop -> C prop
                HSL prop -> C prop
            CAu ca ->
              case ca of
                RGBA prop -> CA prop
                HSLA prop -> CA prop
            Uu u ->
              U u

{-| Opacity style property
-}
opacity : a -> StyleProperty a
opacity o = U (Opacity o)

{-| Height style property
-}
height x = L (Height x)

{-| Width style property
-}
width x = L (Width x)

{-| Left style property
-}
left : a -> StyleProperty a
left x = L (Left x)

{-| Opacity style property
-}
top x = L (Top x)

{-| Opacity style property
-}
right x = L (Right x)

{-| Opacity style property
-}
bottom x = L (Bottom x)

{-| Opacity style property
-}
padding x = L (Padding x)

{-| Opacity style property
-}
paddingLeft x = L (PaddingLeft x)

{-| Opacity style property
-}
paddingRight x = L (PaddingRight x)

{-| Opacity style property
-}
paddingTop x = L (PaddingTop x)

{-| Opacity style property
-}
paddingBottom x = L (PaddingBottom x)

{-| Opacity style property
-}
margin x = L (Margin x)

{-| Opacity style property
-}
marginLeft x = L (MarginLeft x)

{-| Opacity style property
-}
marginRight x = L (MarginRight x)

{-| Opacity style property
-}
marginTop x = L (MarginTop x)

{-| Opacity style property
-}
marginBottom x = L (MarginBottom x)

{-| When a property doesn't have units
-}
noUnit : Unitless a -> StylePropWithUnit a
noUnit unitless = Uu unitless

{-| Pixel units
-}
px : LengthProperty a -> StylePropWithUnit a
px len = Lu (Px len)

{-| Pixel units
-}
unitless : Unitless a -> StyleProperty a
unitless unitless = U unitless

{-| Pixel units
-}
distance : LengthProperty a -> StyleProperty a
distance len = L len



{-| All currently animatable properties.
-}
type AngleProperty a 
          = Rotate a 
          | Rotate3d a a a a 
          | RotateX a 
          | RotateY a 
          | Skew a a 
          | SkewX a  
          | SkewY a 


type LengthProperty a 
        = Height a
        | Width a
        | Left a
        | Top a
        | Right a
        | Bottom a

        | Padding a
        | PaddingLeft a
        | PaddingRight a
        | PaddingTop a
        | PaddingBottom a

        | Margin a
        | MarginLeft a
        | MarginRight a
        | MarginTop a
        | MarginBottom a

        | Translate a a 
        | Translate3d a a a 
        | TranslateX a 
        | TranslateY a 


type Color a 
        = Color a a a
        | BackgroundColor a a a


type ColorAlpha a  
        = ColorA a a a a
        | BackgroundColorA a a a a


type Unitless a 
        = Prop String a String
        | Opacity a
        | Matrix a a a a a a 
        | Matrix3d a a a a a a a a a a a a a a a a 
        | Scale a
        | Scale3d a a a
        | ScaleX a
        | ScaleY a
        | ScaleZ a
        | Perspective a


{-| Units representing length.
-}
type Length a
      = Px (LengthProperty a)
      | Percent (LengthProperty a)
      | Rem (LengthProperty a)
      | Em (LengthProperty a)
      | Ex (LengthProperty a)
      | Ch (LengthProperty a)
      | Vh (LengthProperty a)
      | Vw (LengthProperty a)
      | Vmin (LengthProperty a)
      | Vmax (LengthProperty a)
      | Mm (LengthProperty a)
      | Cm (LengthProperty a)
      | In (LengthProperty a)
      | Pt (LengthProperty a)
      | Pc (LengthProperty a)

{-| Units representing angles.
-}
type Angle a
      = Deg (AngleProperty a)
      | Grad (AngleProperty a)
      | Rad (AngleProperty a)
      | Turn (AngleProperty a)

{-| Units representing color.  Hex codes aren't currently supported, but may be in the future if they're wanted.
-}
type ColorFormat a
      = RGB (Color a)
      | HSL (Color a)


{-| Units representing color that has an alpha channel.
-}
type ColorAlphaFormat a
        = RGBA (ColorAlpha a)
        | HSLA (ColorAlpha a)


{-| StyleActions to be run on an animation. 
You won't be using this type directly, though it may show up in your type signatures.
To perform updates you'll be using the `animate` and `queue` functions
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
animate : StyleAction
animate = Interrupt []


{-| Perform an update on a StyleAnimation
-}
on : StyleAnimation -> StyleAction -> ( StyleAnimation, Effects StyleAction )
on model action = updateStyle action model



{-| Queue up a new animation to be played after the current one.
-}
queue : StyleAction
queue = Queue []



{-| Specify the properties that should be animated

     UI.animate 
         |> UI.duration (0.4*second)
         |> UI.props 
             [ UI.Left UI.Px (UI.to 0) 
             , UI.Opacity (UI.to 1)
             ] 
         |> UI.on model.menuStyle

-}
props : List (StyleProperty Dynamic) -> StyleAction -> StyleAction
props p action = updateOrCreate action (\a -> { a | target = p})
    

{-| Specify a duration for a keyframe.  If a duration isn't specified, the default is 400ms.
-}
duration : Time -> StyleAction -> StyleAction
duration dur action = updateOrCreate action (\a -> { a | duration = dur })
      

{-| Specify an easing function for a keyframe.  It is expected that values should start on 0 and end at 1.  The default is a sinusoidal
in-out.
-}
easing : (Float -> Float) -> StyleAction -> StyleAction
easing ease action = updateOrCreate action (\a -> { a | ease = ease })


{-| Append another keyframe.
-}
andThen : StyleAction -> StyleAction
andThen action = 
          case action of
              Tick _ -> action

              Interrupt frames -> 
                Interrupt (frames ++ [emptyKeyframe])

              Queue frames -> 
                Queue (frames ++ [emptyKeyframe])


-- private
updateOrCreate : StyleAction -> (StyleKeyframe -> StyleKeyframe) -> StyleAction
updateOrCreate action fn =
                let
                  update frames = 
                    case List.reverse frames of
                      [] -> [fn emptyKeyframe] 
                      cur::rem -> List.reverse ((fn cur)::rem)
                in
                 case action of
                    Tick _ -> action

                    Interrupt frames -> 
                      Interrupt (update frames)

                    Queue frames -> 
                      Queue (update frames)


{-| Convenient function to forward an update to a style object contained in a type
 See the Showcase Example to get an idea of whats going on here
-}
forwardTo : (a -> StyleAnimation) -> (a -> StyleAnimation -> a) -> Int -> List a -> StyleAction -> (List a, Effects StyleAction)
forwardTo styleGet styleSet i widgets action = 
              let
                applied = 
                  List.indexedMap 
                        (\j w -> 
                            if j == i then
                              let
                                (newStyle, fx) = updateStyle action (styleGet w)
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
findProp : Style -> StyleProperty a -> Maybe (StylePropWithUnit Static)
findProp state prop =
            let
              findBy fn xs = List.head (List.filter fn xs)
              matchPropID a b = propId a == (propId (removeUnits b))
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
renderProp : StylePropWithUnit Static -> (String, String)
renderProp prop = 
                let
                  unitless = removeUnits prop
                in
                  ( renderName unitless
                  , renderValue prop
                  )

-- private
renderName : StyleProperty a -> String
renderName prop = 
            case prop of
              U (Prop name _ _) -> name
              U (Opacity _) -> "opacity"
              L (Height _) -> "height"
              L (Width _)  -> "width"
              L (Left _)   -> "left"
              L (Right _)  -> "right"
              L (Bottom _) -> "bottom"
              L (Top _)    -> "top"

              L (Padding _)      -> "padding"
              L (PaddingLeft _)  -> "padding-left"
              L (PaddingRight _) -> "padding-right"
              L (PaddingTop _)   -> "padding-top"
              L (PaddingBottom _)-> "padding-bottom"

              L (Margin _)       -> "margin"
              L (MarginLeft _)   -> "margin-left"
              L (MarginRight _)  -> "margin-right"
              L (MarginTop _)    -> "margin-top"
              L (MarginBottom _) -> "margin-bottom"

              C (Color _ _ _)    -> "color" 
              C (BackgroundColor _ _ _) -> "background-color" 

              CA (ColorA _ _ _ _) -> "color"
              CA (BackgroundColorA _ _ _ _) -> "background-color"

              U (Matrix _ _ _ _ _ _) -> "transform"
              U (Matrix3d _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) -> "transform"
              L (Translate _ _) -> "transform"
              L (Translate3d _ _ _) -> "transform"
              L (TranslateX _) -> "transform"
              L (TranslateY _) -> "transform"
              U (Scale _)        -> "transform"
              U (Scale3d _ _ _ ) -> "transform" 
              U (ScaleX _)   -> "transform"
              U (ScaleY _)   -> "transform"
              U (ScaleZ _)   -> "transform"
              R (Rotate _) -> "transform"
              R (Rotate3d _ _ _ _) -> "transform" 
              R (RotateX _)   -> "transform"
              R (RotateY _)   -> "transform"
              R (Skew _ _)   -> "transform"
              R (SkewX _)     -> "transform"
              R (SkewY _)     -> "transform"
              U (Perspective _) -> "transform"


-- private
propId : StyleProperty a -> String
propId prop =
        case prop of
          U (Prop name _ _) -> name
          U (Opacity _) -> "opacity"
          L (Height _) -> "height"
          L (Width _)  -> "width"
          L (Left _)   -> "left"
          L (Right _)  -> "right"
          L (Bottom _) -> "bottom"
          L (Top _)    -> "top"

          L (Padding _)      -> "padding"
          L (PaddingLeft _)  -> "padding-left"
          L (PaddingRight _) -> "padding-right"
          L (PaddingTop _)   -> "padding-top"
          L (PaddingBottom _)-> "padding-bottom"

          L (Margin _)       -> "margin"
          L (MarginLeft _)   -> "margin-left"
          L (MarginRight _)  -> "margin-right"
          L (MarginTop _)    -> "margin-top"
          L (MarginBottom _) -> "margin-bottom"

          C (Color _ _ _)    -> "color" 
          C (BackgroundColor _ _ _) -> "background-color" 

          CA (ColorA _ _ _ _) -> "color"
          CA (BackgroundColorA _ _ _ _) -> "background-color"

          U (Matrix _ _ _ _ _ _) -> "matrix"
          U (Matrix3d _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) -> "matrix3d"
          L (Translate _ _) -> "translate"
          L (Translate3d _ _ _) -> "translate3d" 
          L (TranslateX _) -> "translatex"
          L (TranslateY _) -> "translatey"
          U (Scale _)        -> "scale"
          U (Scale3d _ _ _ ) -> "scale3d"
          U (ScaleX _)   -> "scalex"
          U (ScaleY _)   -> "scaley"
          U (ScaleZ _)   -> "scalez"
          R (Rotate _) -> "rotate"
          R (Rotate3d _ _ _ _) -> "rotate3d"
          R (RotateX _)   -> "rotatex"
          R (RotateY _)   -> "rotatey"
          R (Skew _ _)   -> "skew"
          R (SkewX _)     -> "skewx"
          R (SkewY _)     -> "skewy"
          U (Perspective _) -> "perspective"


-- private
fill : List (StylePropWithUnit Static) -> List (StylePropWithUnit Static) -> List (StylePropWithUnit Static)
fill new existing =
           List.foldl
                    (\x acc ->
                      let
                        unitless = removeUnits x
                      in
                        case findProp acc unitless of
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
                List.concatMap 
                  (\p -> 
                    let
                      existing = findProp prev p
                    in
                      case existing of
                        Nothing -> []
                        Just e ->
                          bakeProp p e eased
                  ) 
                    anim.target
          in
            -- If properties are in previous
            -- but not in the current animation
            -- copy them over as is
            fill style prev





-- private
bakeProp : StyleProperty Dynamic -> StylePropWithUnit Static -> Float -> List (StylePropWithUnit Static)
bakeProp prop prev current =
                case prop of
                    R a ->
                        case prev of
                          Ru p -> [ Ru <| bakeAngle p a current ]
                          _ -> []

                    L a ->
                        case prev of
                          Lu p -> [ Lu <| bakeLength p a current ]
                          _ -> []

                    C a ->
                        case prev of
                          Cu p -> [ Cu <| bakeColor p a current ]
                          _ -> []
                    CA a ->
                        case prev of
                          CAu p -> [ CAu <| bakeColorA p a current ]
                          _ -> []

                    U a ->
                        case prev of
                          Uu p -> [ Uu <| bakeUnitless p a current ]
                          _ -> []



bakeLength : Length Static -> LengthProperty Dynamic -> Float -> Length Static
bakeLength static dynamic current =
              let
                renderLengthProperty prev = 
                      case dynamic of
                        Height to ->
                          case prev of
                            Height from -> Height (to from current)
                            _ -> Height (to 0.0 current)

                        Width to ->
                          case prev of
                            Width from -> Width (to from current)
                            _ -> Width (to 0.0 current)

                        Left to ->
                          case prev of
                            Left from -> Left (to from current)
                            _ -> Left (to 0.0 current)

                        Top to ->
                          case prev of
                            Top from -> Top (to from current)
                            _ -> Top (to 0.0 current)

                        Right to ->
                          case prev of
                            Right from -> Right (to from current)
                            _ -> Right (to 0.0 current)

                        Bottom to ->
                          case prev of
                            Bottom from -> Bottom (to from current)
                            _ -> Bottom (to 0.0 current)

                        Padding to ->
                          case prev of
                            Padding from -> Padding (to from current)
                            _ -> Padding (to 0.0 current)

                        PaddingLeft to ->
                          case prev of
                            PaddingLeft from -> PaddingLeft (to from current)
                            _ -> PaddingLeft (to 0.0 current)

                        PaddingRight to ->
                          case prev of
                            PaddingRight from -> PaddingRight (to from current)
                            _ -> PaddingRight (to 0.0 current)

                        PaddingTop to ->
                          case prev of
                            PaddingTop from -> PaddingTop (to from current)
                            _ -> PaddingTop (to 0.0 current)

                        PaddingBottom to ->
                          case prev of
                            PaddingBottom from -> PaddingBottom (to from current)
                            _ -> PaddingBottom (to 0.0 current)

                        Margin to ->
                          case prev of
                            Margin from -> Margin (to from current)
                            _ -> Margin (to 0.0 current)

                        MarginLeft to ->
                          case prev of
                            MarginLeft from -> MarginLeft (to from current)
                            _ -> MarginLeft (to 0.0 current)

                        MarginRight to ->
                          case prev of
                            MarginRight from -> MarginRight (to from current)
                            _ -> MarginRight (to 0.0 current)

                        MarginTop to ->
                          case prev of
                            MarginTop from -> MarginTop (to from current)
                            _ -> MarginTop (to 0.0 current)

                        MarginBottom to ->
                          case prev of
                            MarginBottom from -> MarginBottom (to from current)
                            _ -> MarginBottom (to 0.0 current)

                        Translate x y ->
                          case prev of
                            Translate fromX fromY -> Translate (x fromX current) (y fromY current)
                            _ -> Translate (x 0.0 current) (y 0.0 current)

                        Translate3d x y z ->
                          case prev of
                            Translate3d fromX fromY fromZ -> 
                              Translate3d (x fromX current) (y fromY current) (z fromZ current)

                            _ -> Translate3d (x 0.0 current) (y 0.0 current) (z 0.0 current)

                        TranslateX to ->
                          case prev of
                            TranslateX from -> TranslateX (to from current)
                            _ -> TranslateX (to 0.0 current)

                        TranslateY to ->
                          case prev of
                            TranslateY from -> TranslateY (to from current)
                            _ -> TranslateY (to 0.0 current)
              in
                case static of
                  Px prop -> Px <| renderLengthProperty prop
                  Percent prop -> Percent <| renderLengthProperty prop                   
                  Rem prop -> Rem <| renderLengthProperty prop 
                  Em prop -> Em <| renderLengthProperty prop 
                  Ex prop -> Ex <| renderLengthProperty prop 
                  Ch prop -> Ch <| renderLengthProperty prop 
                  Vh prop -> Vh <| renderLengthProperty prop 
                  Vw prop -> Vw <| renderLengthProperty prop 
                  Vmin prop -> Vmin <| renderLengthProperty prop 
                  Vmax prop -> Vmax <| renderLengthProperty prop 
                  Mm prop -> Mm <| renderLengthProperty prop 
                  Cm prop -> Cm <| renderLengthProperty prop 
                  In prop -> In <| renderLengthProperty prop 
                  Pt prop -> Pt <| renderLengthProperty prop 
                  Pc prop -> Pc <| renderLengthProperty prop 



bakeAngle : Angle Static -> AngleProperty Dynamic -> Float -> Angle Static
bakeAngle static dynamic current =
            let
              renderAngleProperty prev =
                      case dynamic of
                        Rotate to -> 
                          case prev of
                            Rotate from ->
                               Rotate (to from current)

                            _ -> Rotate (to 0.0 current)

                        Rotate3d x y z a ->  
                           case prev of
                              Rotate3d fromX fromY fromZ fromA -> 
                                Rotate3d (x fromX current) (y fromY current) (z fromZ current) (a fromA current)

                              _ -> 
                                Rotate3d (x 0.0 current) (y 0.0 current) (z 0.0 current) (a 0.0 current)

                        RotateX to ->
                          case prev of
                            RotateX from -> RotateX (to from current)
                            _ -> RotateX (to 0.0 current)

                        RotateY to ->
                          case prev of
                            RotateY from -> RotateY (to from current)
                            _ -> RotateY (to 0.0 current)

                        Skew x y ->
                          case prev of
                            Skew fromX fromY -> Skew (x fromX current) (y fromY current)
                            _ -> Skew (x 0 current) (y 0 current)

                        SkewX to ->
                          case prev of
                            SkewX from -> SkewX (to from current)
                            _ -> SkewX (to 0.0 current)

                        SkewY to ->
                          case prev of
                            SkewY from -> SkewY (to from current)
                            _ -> SkewY (to 0.0 current) 
            in 
              case static of
                Deg prev ->
                  Deg <| renderAngleProperty prev

                Grad prev ->
                  Grad <| renderAngleProperty prev

                Rad prev ->
                  Rad <| renderAngleProperty prev

                Turn prev ->
                  Turn <| renderAngleProperty prev




bakeColor : ColorFormat Static -> Color Dynamic -> Float -> ColorFormat Static
bakeColor static dynamic current = 
              let
                renderColor prev =
                        case dynamic of
                          Color x y z ->
                            case prev of
                              Color fromX fromY fromZ -> 
                                Color (x fromX current) (y fromY current) (z fromZ current)

                              _ -> Color (x 0.0 current) (y 0.0 current) (z 0.0 current)

                          BackgroundColor x y z ->
                            case prev of
                              BackgroundColor fromX fromY fromZ -> 
                                BackgroundColor (x fromX current) (y fromY current) (z fromZ current)

                              _ -> BackgroundColor (x 0.0 current) (y 0.0 current) (z 0.0 current)
              in
                case static of
                  RGB prev -> 
                    RGB <| renderColor prev

                  HSL prev -> 
                    HSL <| renderColor prev

bakeColorA : ColorAlphaFormat Static -> ColorAlpha Dynamic -> Float -> ColorAlphaFormat Static
bakeColorA static dynamic current = 
              let
                renderColor prev =
                      case dynamic of
                        ColorA x y z a ->
                          case prev of
                            ColorA fromX fromY fromZ fromA -> 
                              ColorA (x fromX current) (y fromY current) (z fromZ current) (a fromA current)

                            _ -> ColorA (x 0.0 current) (y 0.0 current) (z 0.0 current) (a 0.0 current)

                        BackgroundColorA x y z a ->
                          case prev of
                            BackgroundColorA fromX fromY fromZ fromA -> 
                              BackgroundColorA (x fromX current) (y fromY current) (z fromZ current) (a fromA current)

                            _ -> BackgroundColorA (x 0.0 current) (y 0.0 current) (z 0.0 current) (a 0.0 current)
              in
                case static of
                  RGBA prev -> 
                    RGBA <| renderColor prev

                  HSLA prev -> 
                    HSLA <| renderColor prev

bakeUnitless : Unitless Static -> Unitless Dynamic -> Float -> Unitless Static
bakeUnitless prev dynamic current = 
                        case dynamic of
                          Prop name to unit ->
                            case prev of
                              Prop _ from _ -> Prop name (to from current) unit
                              _ -> Prop name (to 0.0 current) unit

                          Opacity to ->
                            case prev of
                              Opacity from -> Opacity (to from current)
                              _ -> Opacity (to 0.0 current)

                          Scale to ->
                            case prev of
                              Scale from -> Scale (to from current)
                              _ -> Scale (to 0.0 current)

                          Scale3d x y z ->
                            case prev of
                              Scale3d fromX fromY fromZ -> 
                                Scale3d (x fromX current) (y fromY current) (z fromZ current)

                              _ -> Scale3d (x 0.0 current) (y 0.0 current) (z 0.0 current)

                          ScaleX to ->
                              case prev of
                                ScaleX from -> ScaleX (to from current)
                                _ -> ScaleX (to 0.0 current)

                          ScaleY to ->
                              case prev of
                                ScaleY from -> ScaleY (to from current)
                                _ -> ScaleY (to 0.0 current)

                          ScaleZ to ->
                              case prev of
                                ScaleZ from -> ScaleZ (to from current)
                                _ -> ScaleZ (to 0.0 current)

                          Perspective to ->
                              case prev of
                                Perspective from -> Perspective (to from current)
                                _ -> Perspective (to 0.0 current)

                          Matrix a b c x y z -> 
                            let
                              val x1 x2 = x1 x2 current
                            in
                              case prev of
                                Matrix a2 b2 c2 x2 y2 z2 ->
                                  Matrix (val a a2) (val b b2) (val c c2)
                                         (val x x2) (val y y2) (val z z2)

                                _ -> 
                                  Matrix (val a 0.0) (val b 0.0) (val c 0.0)
                                         (val x 0.0) (val y 0.0) (val z 0.0)


                          Matrix3d a b c d e f g h i j k l m n o p -> 
                              let
                                val x1 x2 = x1 x2 current
                              in
                                case prev of
                                  Matrix3d a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 o2 p2 -> 
                                       Matrix3d (val a a2) (val b b2) (val c c2) (val d d2) 
                                                (val e e2) (val f f2) (val g g2) (val h h2) 
                                                (val i i2) (val j j2) (val k k2) (val l l2) 
                                                (val m m2) (val n n2) (val o o2) (val p p2)

                                  _ -> 
                                       Matrix3d (val a 0.0) (val b 0.0) (val c 0.0) (val d 0.0) 
                                                (val e 0.0) (val f 0.0) (val g 0.0) (val h 0.0) 
                                                (val i 0.0) (val j 0.0) (val k 0.0) (val l 0.0) 
                                                (val m 0.0) (val n 0.0) (val o 0.0) (val p 0.0)






renderValue : StylePropWithUnit Static -> String
renderValue prop = 
          case prop of
            Ru a ->
              case a of
                Deg angle -> renderAngleValue angle "deg"
                Grad angle -> renderAngleValue angle "grad"
                Rad angle -> renderAngleValue angle "rad"
                Turn angle -> renderAngleValue angle "turn"
            Lu l ->
              case l of
                Px prop -> renderLengthValue prop "px"
                Percent prop -> renderLengthValue prop "%"
                Rem prop -> renderLengthValue prop "rem"
                Em prop -> renderLengthValue prop "em"
                Ex prop -> renderLengthValue prop "ex"
                Ch prop -> renderLengthValue prop "ch"
                Vh prop -> renderLengthValue prop "vh"
                Vw prop -> renderLengthValue prop "vw"
                Vmin prop -> renderLengthValue prop "vmin"
                Vmax prop -> renderLengthValue prop "vmax"
                Mm prop -> renderLengthValue prop "mm"
                Cm prop -> renderLengthValue prop "cm"
                In prop -> renderLengthValue prop "in"
                Pt prop -> renderLengthValue prop "pt"
                Pc prop -> renderLengthValue prop "pc"
            Cu c ->
              case c of
                RGB prop -> renderColor prop "rgb"
                HSL prop -> renderColor prop "hsl"
            CAu ca ->
              case ca of
                RGBA prop -> renderAlphaColor prop "rgba"
                HSLA prop -> renderAlphaColor prop "hsla"
            Uu u ->
              renderUnitlessValue u


renderAngleValue : AngleProperty Static -> String -> String
renderAngleValue prop unit =
                case prop of
                  Rotate x -> 
                      "rotate(" ++ toString x ++ unit ++ ")"

                  Rotate3d x y z a ->
                       "rotate3d(" ++ toString x 
                            ++ "," ++ toString y 
                            ++ "," ++ toString z 
                            ++ "," ++ toString a ++ unit 
                            ++ ")"

                  RotateX x ->
                       "rotateX(" ++ toString x ++ unit ++ ")"

                  RotateY y ->
                       "rotateY(" ++ toString y ++ unit ++ ")"

                  Skew x y ->
                      "skew(" ++ toString x ++ unit 
                       ++ "," ++ toString y ++ unit 
                       ++ ")"

                  SkewX x -> 
                      "skewX(" ++ toString x ++ unit ++ ")"

                  SkewY y ->
                       "skewY(" ++ toString y ++ unit ++ ")"

renderLengthValue : LengthProperty Static -> String -> String
renderLengthValue prop unit =
                case prop of
                  Height a -> toString a ++ unit
                  Width a -> toString a ++ unit
                  Left a -> toString a ++ unit
                  Top a -> toString a ++ unit
                  Right a -> toString a ++ unit
                  Bottom a -> toString a ++ unit

                  Padding a -> toString a ++ unit
                  PaddingLeft a -> toString a ++ unit
                  PaddingRight a -> toString a ++ unit
                  PaddingTop a -> toString a ++ unit
                  PaddingBottom a -> toString a ++ unit

                  Margin a -> toString a ++ unit
                  MarginLeft a -> toString a ++ unit
                  MarginRight a -> toString a ++ unit
                  MarginTop a -> toString a ++ unit
                  MarginBottom a -> toString a ++ unit

                  Translate x y -> "translate(" ++ toString x ++ unit ++
                                            "," ++ toString y ++ unit ++ ")"
                  Translate3d x y z ->  
                          "translate3d(" ++ toString x ++ unit 
                                  ++ "," ++ toString y ++ unit 
                                  ++ "," ++ toString z ++ unit 
                                  ++ ")"
                  TranslateX a -> "translateX(" ++ toString a ++ unit ++ ")"
                  TranslateY a -> "translateY(" ++ toString a ++ unit ++ ")"


renderColor : Color Static -> String -> String
renderColor prop format = 
            let
              asInt a = toString (round a) 
            in
              case prop of
                Color a b c -> 
                    format ++ "(" ++ asInt a 
                                  ++ asInt b 
                                  ++ asInt c 
                                  ++ ")"

                BackgroundColor a b c -> 
                    format ++ "(" ++ asInt a 
                                  ++ asInt b 
                                  ++ asInt c 
                                  ++ ")"

renderAlphaColor : ColorAlpha Static -> String -> String
renderAlphaColor prop format = 
            let
              asInt a = toString (round a) 
            in
              case prop of
                ColorA a b c z -> 
                    format ++ "(" ++ asInt a 
                                  ++ asInt b 
                                  ++ asInt c 
                                  ++ toString z 
                                  ++ ")"

                BackgroundColorA a b c z -> 
                    format ++ "(" ++ asInt a 
                                  ++ asInt b 
                                  ++ asInt c 
                                  ++ toString z 
                                  ++ ")"


renderUnitlessValue : Unitless Static -> String
renderUnitlessValue prop = 
                let
                  renderList xs = "(" ++ (String.concat 
                                        <| List.intersperse "," 
                                        <| List.map toString xs) ++ ")"
                in
                  case prop of
                    Prop _ a u -> toString a ++ u
                    Opacity a -> toString a
                    
                    Scale a -> "scale(" ++ toString a ++ ")"
                    Scale3d x y z -> 
                        "scale3d(" ++ (toString x) 
                            ++ "," ++ (toString y) 
                            ++ "," ++ (toString z) 
                            ++ ")"
                    ScaleX a -> "scaleX(" ++ toString a ++ ")"
                    ScaleY a -> "scaleY(" ++ toString a ++ ")"
                    ScaleZ a -> "scaleZ(" ++ toString a ++ ")"
                    Perspective a -> "perspective(" ++ toString a ++ ")"
                    Matrix a b c x y z -> 
                          "matrix" ++ 
                            (renderList [a,b,c,x,y,z])

                    Matrix3d a b c d e f g h i j k l m n o p -> 
                        "matrix3d" ++ 
                          (renderList [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p])









