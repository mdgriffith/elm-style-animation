module Html.Animation 
    ( Animation
    , Action
    , Staggered
    , StyleProperty (..)
    , Length (..), Angle (..) 
    , ColorFormat (..), ColorAlphaFormat (..)
    , init
    , update
    , render
    , animate, queue
    , stagger
    , on
    , props, duration, delay, easing
    , andThen, forwardTo, forwardToAll
    , to, add, minus
    ) where

{-| This library is for animating css properties and is meant to work well with elm-html.

The easiest way to get started with this library is to check out the examples that are included with the [source code](https://github.com/mdgriffith/elm-html-animation).

Once you have the basic structure of how to use this library, you can refer to this documentation to fill any gaps.


# Base Definitions
@docs Animation, Action

# Creating an animation
@docs animate, queue, props, duration, easing, andThen, on

# Animating Properties

These functions specify the value for a StyleProperty

@docs to, add, minus

You can substitute a custom function to use instead of `to`, `add` or `minus`.  This could be useful if to do something like animate along a path.

The function needs to have the following signature.

    Float -> Float -> Float

Where the first argument is the existing property value and the second argument represents the current time (between 0.0 and 1.0).  
Finally the function would return what the current value should be for the property.


# Render a Animation into CSS
@docs render

# Setting the starting style
@docs init

# Update a Style
@docs update

# All Animatable Style Properties
@docs StyleProperty

# Units
@docs Length, Angle, ColorFormat, ColorAlphaFormat

# Managing a list of styled widgets
@docs forwardTo, forwardToAll

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
type Animation = A Model


type alias Static = Float


type alias Dynamic
         = (Float -> Float -> Float)

{-| Represent a CSS style as a list of style properties with concrete values.
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
            , delay : Time
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

        | MaxHeight a Length
        | MaxWidth a Length
        | MinHeight a Length
        | MinWidth a Length

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

        | BorderWidth a Length
        | BorderRadius a Length
        | BorderTopLeftRadius a Length
        | BorderTopRightRadius a Length
        | BorderBottomLeftRadius a Length
        | BorderBottomRightRadius a Length

        | LetterSpacing a Length
        | LineHeight a Length

        | BackgroundPosition a a Length


        -- Color
        | Color ColorFormat a a a
        | BackgroundColor ColorFormat a a a
        | BorderColor ColorFormat a a a

        | ColorA ColorAlphaFormat a a a a
        | BackgroundColorA ColorAlphaFormat a a a a
        | BorderColorA ColorAlphaFormat a a a a

        | TransformOrigin a a a Length

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


{-| Actions to be run on an animation. 
You won't be constructing using this type directly, though it may show up in your type signatures.

To start animations you'll be using the `animate` and `queue` functions
-}
type Action 
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
emptyKeyframe = { target = []
                , duration = defaultDuration
                , ease = defaultEasing 
                , delay = 0.0
                }

{-| Create an initial style for your init model.

__Note__ All properties that you animate must be present in the init or else that property won't be animated.

-}
init : Style -> Animation
init sty = 
            let
              deduped = List.foldr 
                              (\x acc ->
                                  if List.any (\y -> propId x == propId y 
                                                 && renderName x /= "transform") acc then
                                    acc
                                  else
                                    x :: acc 
                              ) [] sty
            in
              A { empty | previous = deduped }


-- private
defaultDuration : Float
defaultDuration = 0.4 * second

-- private
defaultEasing : Float -> Float
defaultEasing x = (1 - cos (pi*x))/2



{-| Update an animation.  This is only used to 'forward' updates to a style.  So, it will probably only show up once in your code.  See any of the examples at [https://github.com/mdgriffith/elm-html-animation](https://github.com/mdgriffith/elm-html-animation)
-}
update : Action -> Animation -> ( Animation, Effects Action )
update action (A model) =
       
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
                  if newElapsed >= (current.duration + current.delay) then
                    let
                      anims = 
                        case remaining of
                          Nothing -> []
                          Just a -> a

                      previous = 
                          bake (current.duration + current.delay) current model.previous

                      resetElapsed = 
                          newElapsed - (current.duration + current.delay)
                              
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


{-| Begin describing an animation.  This animation will cleanly interrupt any animation that is currently running.

      UI.animate 
         |> UI.duration (0.4*second)
         |> UI.props 
             [ UI.Left UI.Px (UI.to 0) 
             , UI.Opacity (UI.to 1)
             ] 
         |> UI.on model.style

-}
animate : Staggered
animate = Unstaggered (Interrupt [])


{-| The same as `animate` but instead of interrupting the current animation, this will queue up after the current animation is finished.

      UI.queue
         |> UI.duration (0.4*second)
         |> UI.props 
             [ UI.Left UI.Px (UI.to 0) 
             , UI.Opacity (UI.to 1)
             ] 
         |> UI.on model.style

-}
queue : Staggered
queue = Unstaggered (Queue [])


type Staggered 
      = Staggered (Float -> Staggered)
      | Unstaggered Action


stagger : (Float -> Staggered) -> Staggered
stagger = Staggered
          


{-| Apply an update to a Animation model.  This is used at the end of constructing an animation.

     UI.animate 
         |> UI.duration (0.4*second)
         |> UI.props 
             [ UI.Left UI.Px (UI.to 0) 
             , UI.Opacity (UI.to 1)
             ] 
         |> UI.on model.style

-}
on : Animation -> Staggered -> ( Animation, Effects Action )
on model action = update (resolveStagger action 0) model


resolveStagger : Staggered -> Int -> Action
resolveStagger stag i =
                  let
                    f = toFloat i
                  in
                    case stag of
                      Unstaggered a -> a
                      Staggered s -> resolveStagger (s f) i

{-|  Can be used in place of `on`.  Instead of applying an update directly to a Animation model,
you can forward the update to a specific element in a list that has a Animation model.

To use this function, you'll need to supply a getter and a setter function for getting and setting the style model.

So, for a model like the following

    type alias Model = { widgets : List Widget }

    type alias Widget = 
              { style : UI.Animation
              }
You'd probably want to create a specialized version of `forwardTo`.

    forwardToWidget = UI.forwardTo 
                        .style -- widget style getter
                        (\w style -> { w | style = style }) -- widget style setter

Which you can then use to apply an animation to a widget in a list.

    (widgets, fx) = 
            UI.animate
                |> UI.duration (5*second)
                |> UI.props 
                    [ UI.Opacity (UI.to 0)  
                    ] 
                |> forwardToWidget i model.widgets
                -- Where i is the index of the widget to update.

-}
forwardTo : (Int -> Staggered -> b) -> (a -> Animation) -> (a -> Animation -> a) -> Int -> List a -> Staggered -> (List a, Effects b)
forwardTo toAction styleGet styleSet i widgets action = 
              let
                (widgets, effects) = 
                    List.unzip 
                      <| List.indexedMap 
                            (\j widget -> 
                                if j == i then
                                  let
                                    (newStyle, fx) = update (resolveStagger action i) (styleGet widget)
                                  in
                                    (styleSet widget newStyle, Effects.map (\a -> toAction i (Unstaggered a)) fx)
                                else
                                  (widget, Effects.none)
                            ) widgets
              in
                (widgets, Effects.batch effects)

{-|  Same as `forwardTo`, except it applies an update to every member of the list.

-}
forwardToAll : (Int -> Staggered -> b) -> (a -> Animation) -> (a -> Animation -> a) -> List a -> Staggered -> (List a, Effects b)
forwardToAll toAction styleGet styleSet widgets action = 
              let
                (widgets, effects) = 
                        List.unzip 
                          <| List.indexedMap 
                              (\i widget -> 
                                let
                                  (newStyle, fx) = update (resolveStagger action i) (styleGet widget)
                                in
                                  (styleSet widget newStyle, Effects.map (\a -> toAction i (Unstaggered a)) fx)
                              ) widgets
              in
                (widgets, Effects.batch effects)



{-| Specify the properties that should be animated

     UI.animate 
         |> UI.duration (0.4*second)
         |> UI.props 
             [ UI.Left UI.Px (UI.to 0) 
             , UI.Opacity (UI.to 1)
             ] 
         |> UI.on model.style

-}
props : List (StyleProperty Dynamic) -> Staggered -> Staggered
props p action = updateOrCreate action (\a -> { a | target = p})
    

{-| Optionally specify a duration.  The default is 400ms.
-}
duration : Time -> Staggered -> Staggered
duration dur action = updateOrCreate action (\a -> { a | duration = dur })
  

{-| Optionally specify a delay.  The default is 0.
-}
delay : Time -> Staggered -> Staggered
delay dur action = updateOrCreate action (\a -> { a | delay = dur })


{-| Opitionally specify an easing function.  It is expected that values should match up at the beginning and end.  So, f 0 == 0 and f 1 == 1.  The default easing is sinusoidal
in-out.
-}
easing : (Float -> Float) -> Staggered -> Staggered
easing ease action = updateOrCreate action (\a -> { a | ease = ease })


{-| Append another keyframe.  This is used for multistage animations.  For example, to cycle through colors, we'd use the following:

      UI.animate 
              |> UI.props 
                  [ UI.BackgroundColorA 
                        UI.RGBA (UI.to 100) (UI.to 100) (UI.to 100) (UI.to 1.0)  
                  ] 
          |> UI.andThen -- create a new keyframe
              |> UI.duration (1*second)
              |> UI.props 
                  [ UI.BackgroundColorA 
                        UI.RGBA (UI.to 178) (UI.to 201) (UI.to 14) (UI.to 1.0) 
                  ] 
          |> UI.andThen 
              |> UI.props 
                  [ UI.BackgroundColorA 
                        UI.RGBA (UI.to 58) (UI.to 40) (UI.to 69) (UI.to 1.0) 
                  ] 
          |> UI.on model.style
-}
andThen : Staggered -> Staggered
andThen stag = 
          case stag of 
            Staggered s -> Staggered s

            Unstaggered action ->
              case action of
                  Tick _ -> Unstaggered action

                  Interrupt frames -> 
                    Unstaggered <|
                      Interrupt (frames ++ [emptyKeyframe])

                  Queue frames -> 
                    Unstaggered <|
                      Queue (frames ++ [emptyKeyframe])


-- private
updateOrCreate : Staggered -> (StyleKeyframe -> StyleKeyframe) -> Staggered
updateOrCreate stag fn =
                case stag of 
                  Staggered s -> Staggered s

                  Unstaggered action ->
                    let
                      update frames = 
                        case List.reverse frames of
                          [] -> [fn emptyKeyframe] 
                          cur::rem -> List.reverse ((fn cur)::rem)
                    in
                     case action of
                        Tick _ -> Unstaggered action

                        Interrupt frames -> 
                          Unstaggered <| Interrupt (update frames)

                        Queue frames -> 
                          Unstaggered <| Queue (update frames)




{-| Animate a StyleProperty to a value.

-}
to : Float -> Float -> Float -> Float
to target from current = ((target-from) * current) + from

{-| Animate a StyleProperty by adding to its existing value

-}
add : Float -> Float -> Float -> Float
add mod from current = 
        let
          target = from + mod
        in
          to target from current

{-| Animate a StyleProperty by subtracting to its existing value

-}
minus : Float -> Float -> Float -> Float
minus mod from current = 
        let
          target = from - mod
        in
          to target from current



-- private
-- propCount refers to the how many times a property shows up 
-- in the original list that prop is being pulled from
findProp : Style -> StyleProperty a -> Int -> Maybe (StyleProperty Static)
findProp state prop propCount =
            let
              findBy fn xs = List.head 
                          <| List.drop propCount 
                          <| List.filter fn xs
                              
              matchPropID a b = propId a == propId b
            in 
              findBy (matchPropID prop) state


{-| Render into concrete css that can be directly applied to 'style' in elm-html

    div [ style (UI.render widget.style) ] [ ]

-}
render : Animation -> List (String, String)
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

              MaxHeight _ _ -> "max-height"
              MaxWidth _ _  -> "max-width"
              MinHeight _ _ -> "min-height"
              MinWidth _ _  -> "min-width"

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

              BorderWidth _ _ -> "border-width"
              BorderRadius _ _ -> "border-radius"
              BorderTopLeftRadius _ _ -> "border-top-left-radius"
              BorderTopRightRadius _ _ -> "border-top-right-radius"
              BorderBottomLeftRadius _ _ -> "border-bottom-left-radius"
              BorderBottomRightRadius _ _ -> "border-bottom-right-radius"

              LetterSpacing _ _ -> "letter-spacing"
              LineHeight _ _ -> "line-height"

              BackgroundPosition _ _ _ -> "background-position"

              TransformOrigin _ _ _ _ -> "transform-origin"

              Color _ _ _ _    -> "color"
              BackgroundColor _ _ _ _ -> "background-color"
              BorderColor _ _ _ _ -> "border-color"

              ColorA _ _ _ _ _ -> "color"
              BackgroundColorA _ _ _ _ _ -> "background-color"
              BorderColorA _ _ _ _ _ -> "border-color"

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
           List.foldr
                    (\x acc ->
                      -- need to know the propIndex of x, meaning how many times it's shown up already. 
                      let
                        xI = List.foldr 
                                    (\x2 count -> 
                                        if propId x == propId x2 then
                                          count + 1
                                        else
                                          count
                                    ) 0 acc
                      in
                        case findProp new x xI of
                          Nothing ->   x :: acc 
                          Just newX -> newX :: acc
                    ) [] existing

-- private
-- Converts an animation into a Style that can be rendered.
bake : Time -> StyleKeyframe -> Style -> Style
bake elapsed anim prev = 
          let
            percentComplete = 
                 (elapsed - anim.delay) / anim.duration

            eased = 
                 anim.ease percentComplete

            style = 
               List.foldr
                    (\x acc ->
                      -- need to know how many times x has shown up already. 
                      let
                        xI = List.foldr 
                                  (\x2 count -> 
                                      if propId x == propId x2 then
                                        count + 1
                                      else
                                        count
                                  ) 0 acc
                      in
                        case findProp prev x xI of
                          Nothing ->   acc 
                          Just prevX -> (bakeProp x prevX eased) :: acc
                    ) [] anim.target

          in
            if percentComplete > 0.0 then
              -- If properties are in previous
              -- but not in the current animation
              -- copy them over as is
              fill style prev
            else
              prev

-- private
bakeProp : StyleProperty Dynamic -> StyleProperty Static -> Float -> StyleProperty Static
bakeProp prop prev current =
            let
              val from fn = fn from current
            in
              case prop of
                Prop name to unit -> 
                  let
                    from =
                      case prev of
                        Prop _ x _ -> x
                        _ -> 0.0
                  in
                    Prop name (val from to) unit


                Opacity to -> 
                  let
                    from =
                      case prev of
                        Opacity x -> x
                        _ -> 0.0
                  in
                    Opacity (val from to)


                Height to unit -> 
                  let
                    from =
                      case prev of
                        Height x _ -> x
                        _ -> 0.0
                  in
                    Height (val from to) unit


                Width to unit -> 
                  let
                    from =
                      case prev of
                        Width x _ -> x
                        _ -> 0.0
                  in
                    Width (val from to) unit


                Left to unit -> 
                  let
                    from =
                      case prev of
                        Left x _ -> x
                        _ -> 0.0
                  in
                    Left (val from to) unit


                Top to unit -> 
                  let
                    from =
                      case prev of
                        Top x _ -> x
                        _ -> 0.0
                  in
                    Top (val from to) unit


                Right to unit ->  
                  let
                    from =
                      case prev of
                        Right x _ -> x
                        _ -> 0.0
                  in
                    Right (val from to) unit


                Bottom to unit -> 
                  let
                    from =
                      case prev of
                        Bottom x _ -> x
                        _ -> 0.0
                  in
                    Bottom (val from to) unit

                MaxHeight to unit -> 
                  let
                    from =
                      case prev of
                        MaxHeight x _ -> x
                        _ -> 0.0
                  in
                    MaxHeight (val from to) unit

                MaxWidth to unit  -> 
                  let
                    from =
                      case prev of
                        MaxWidth x _ -> x
                        _ -> 0.0
                  in
                    MaxWidth (val from to) unit

                MinHeight to unit -> 
                  let
                    from =
                      case prev of
                        MinHeight x _ -> x
                        _ -> 0.0
                  in
                    MinHeight (val from to) unit

                MinWidth to unit  -> 
                  let
                    from =
                      case prev of
                        MinWidth x _ -> x
                        _ -> 0.0
                  in
                    MinWidth (val from to) unit



                Padding to unit -> 
                  let
                    from =
                      case prev of
                        Padding x _ -> x
                        _ -> 0.0
                  in
                    Padding (val from to) unit


                PaddingLeft to unit -> 
                  let
                    from =
                      case prev of
                        PaddingLeft x _ -> x
                        _ -> 0.0
                  in
                    PaddingLeft (val from to) unit


                PaddingRight to unit  -> 
                  let
                    from =
                      case prev of
                        PaddingRight x _ -> x
                        _ -> 0.0
                  in
                    PaddingRight (val from to) unit


                PaddingTop to unit -> 
                  let
                    from =
                      case prev of
                        PaddingTop x _ -> x
                        _ -> 0.0
                  in
                    PaddingTop (val from to) unit


                PaddingBottom to unit -> 
                  let
                    from =
                      case prev of
                        PaddingBottom x _ -> x
                        _ -> 0.0
                  in
                    PaddingBottom (val from to) unit


                Margin to unit ->
                  let
                    from =
                      case prev of
                        Margin x _ -> x
                        _ -> 0.0
                  in
                    Margin (val from to) unit


                MarginLeft to unit   -> 
                  let
                    from =
                      case prev of
                        MarginLeft x _ -> x
                        _ -> 0.0
                  in
                    MarginLeft (val from to) unit


                MarginRight to unit  -> 
                  let
                    from =
                      case prev of
                        MarginRight x _ -> x
                        _ -> 0.0
                  in
                    MarginRight (val from to) unit


                MarginTop to unit -> 
                  let
                    from =
                      case prev of
                        MarginTop x _ -> x
                        _ -> 0.0
                  in
                    MarginTop (val from to) unit 


                MarginBottom to unit ->  
                  let
                    from =
                      case prev of
                        MarginBottom x _ -> x
                        _ -> 0.0
                  in
                    MarginBottom (val from to) unit

                BorderWidth to unit ->  
                  let
                    from =
                      case prev of
                        BorderWidth x _ -> x
                        _ -> 0.0
                  in
                    BorderWidth (val from to) unit

                BorderRadius to unit ->  
                  let
                    from =
                      case prev of
                        BorderRadius x _ -> x
                        _ -> 0.0
                  in
                    BorderRadius (val from to) unit

                BorderTopLeftRadius to unit ->  
                  let
                    from =
                      case prev of
                        BorderTopLeftRadius x _ -> x
                        _ -> 0.0
                  in
                    BorderTopLeftRadius (val from to) unit

                BorderTopRightRadius to unit ->  
                  let
                    from =
                      case prev of
                        BorderTopRightRadius x _ -> x
                        _ -> 0.0
                  in
                    BorderTopRightRadius (val from to) unit

                BorderBottomLeftRadius to unit ->  
                  let
                    from =
                      case prev of
                        BorderBottomLeftRadius x _ -> x
                        _ -> 0.0
                  in
                    BorderBottomLeftRadius (val from to) unit

                BorderBottomRightRadius to unit ->  
                  let
                    from =
                      case prev of
                        BorderBottomRightRadius x _ -> x
                        _ -> 0.0
                  in
                    BorderBottomRightRadius (val from to) unit


                LetterSpacing to unit ->  
                  let
                    from =
                      case prev of
                        LetterSpacing x _ -> x
                        _ -> 0.0
                  in
                    LetterSpacing (val from to) unit

                LineHeight to unit ->  
                  let
                    from =
                      case prev of
                        LineHeight x _ -> x
                        _ -> 0.0
                  in
                    LineHeight (val from to) unit


                BackgroundPosition x y unit ->  
                      case prev of
                        BackgroundPosition xFrom yFrom _ -> 
                              BackgroundPosition (val xFrom x) (val yFrom y) unit
                        _ -> 
                          BackgroundPosition (val 0.0 x) (val 0.0 y) unit
                 


                Color unit x y z    -> 
                  let
                    (xFrom, yFrom, zFrom) =
                      case prev of
                        Color _ x1 y1 z1 -> (x1, y1, z1)
                        _ -> (0.0, 0.0, 0.0)
                  in
                    Color unit (val xFrom x) (val yFrom y) (val zFrom z)

                BorderColor unit x y z    -> 
                  let
                    (xFrom, yFrom, zFrom) =
                      case prev of
                        BorderColor _ x1 y1 z1 -> (x1, y1, z1)
                        _ -> (0.0, 0.0, 0.0)
                  in
                    BorderColor unit (val xFrom x) (val yFrom y) (val zFrom z)


                BackgroundColor unit x y z -> 
                  let
                    (xFrom, yFrom, zFrom) =
                      case prev of
                        BackgroundColor _ x1 y1 z1 -> (x1, y1, z1)
                        _ -> (0.0, 0.0, 0.0)
                  in
                    BackgroundColor unit (val xFrom x) (val yFrom y) (val zFrom z)


                ColorA unit x y z a -> 
                  let
                    (xFrom, yFrom, zFrom, aFrom) =
                      case prev of
                        ColorA _ x1 y1 z1 a1 -> (x1, y1, z1, a1)
                        _ -> (0.0, 0.0, 0.0, 0.0)
                  in
                    ColorA unit (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a)

                BorderColorA unit x y z a -> 
                  let
                    (xFrom, yFrom, zFrom, aFrom) =
                      case prev of
                        BorderColorA _ x1 y1 z1 a1 -> (x1, y1, z1, a1)
                        _ -> (0.0, 0.0, 0.0, 0.0)
                  in
                    BorderColorA unit (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a)


                BackgroundColorA unit x y z a -> 
                  let
                    (xFrom, yFrom, zFrom, aFrom) =
                      case prev of
                        BackgroundColorA _ x1 y1 z1 a1 -> (x1, y1, z1, a1)
                        _ -> (0.0, 0.0, 0.0, 0.0)
                  in
                    BackgroundColorA unit (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a)


                TransformOrigin x y z unit ->
                  let
                    (xFrom, yFrom, zFrom) =
                      case prev of
                        TransformOrigin x1 y1 z1 _ -> (x1, y1, z1)
                        _ -> (0.0, 0.0, 0.0)
                  in
                    TransformOrigin (val xFrom x) (val yFrom y) (val zFrom z) unit


                Translate x y unit -> 
                  let
                    (xFrom, yFrom) =
                      case prev of
                        Translate x1 y1 _ -> (x1, y1)
                        _ -> (0.0, 0.0)
                  in
                    Translate (val xFrom x) (val yFrom y) unit
                    

                Translate3d x y z unit -> 
                  let
                    (xFrom, yFrom, zFrom) =
                      case prev of
                        Translate3d x1 y1 z1 _ -> (x1, y1, z1)
                        _ -> (0.0, 0.0, 0.0)
                  in
                    Translate3d (val xFrom x) (val yFrom y) (val zFrom z) unit
                  

                TranslateX to unit -> 
                  let
                    from =
                      case prev of
                        TranslateX x _ -> x
                        _ -> 0.0
                  in
                    TranslateX (val from to) unit


                TranslateY to unit -> 
                  let
                    from =
                      case prev of
                        TranslateY x _ -> x
                        _ -> 0.0
                  in
                    TranslateY (val from to) unit


                Scale to -> 
                  let
                    from =
                      case prev of
                        Scale x -> x
                        _ -> 0.0
                  in
                     Scale (val from to)


                Scale3d x y z -> 
                  let
                    (xFrom, yFrom, zFrom) =
                      case prev of
                        Scale3d x1 y1 z1 -> (x1, y1, z1)
                        _ -> (0.0, 0.0, 0.0)
                  in
                    Scale3d (val xFrom x) (val yFrom y) (val zFrom z)
                   

                ScaleX to -> 
                  let
                    from =
                      case prev of
                        ScaleX x -> x
                        _ -> 0.0
                  in
                    ScaleX (val from to)


                ScaleY to -> 
                  let
                    from =
                      case prev of
                        ScaleY x -> x
                        _ -> 0.0
                  in
                    ScaleY (val from to)


                ScaleZ to -> 
                  let
                    from =
                      case prev of
                        ScaleZ x -> x
                        _ -> 0.0
                  in
                    ScaleZ (val from to)


                Rotate to unit -> 
                  let
                    from =
                      case prev of
                        Rotate x _ -> x
                        _ -> 0.0
                  in
                    Rotate (val from to) unit


                Rotate3d x y z a unit ->
                  let
                    (xFrom, yFrom, zFrom, aFrom) =
                      case prev of
                        Rotate3d x1 y1 z1 a1 _ -> (x1, y1, z1, a1)
                        _ -> (0.0, 0.0, 0.0, 0.0)
                  in 
                    Rotate3d (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a) unit
                   

                RotateX to unit -> 
                  let
                    from =
                      case prev of
                        RotateX x _ -> x
                        _ -> 0.0
                  in
                    RotateX (val from to) unit

                RotateY to unit -> 
                  let
                    from =
                      case prev of
                        RotateY x _ -> x
                        _ -> 0.0
                  in
                    RotateY (val from to) unit


                Skew x y unit ->
                  let
                    (xFrom, yFrom) =
                      case prev of
                        Skew x y _ -> (x, y)
                        _ -> (0.0, 0.0)
                  in
                    Skew (val xFrom x) (val yFrom y) unit 
                    

                SkewX to unit -> 
                  let
                     from =
                      case prev of
                        SkewX x _ -> x
                        _ -> 0.0
                  in
                    SkewX (val from to) unit

                SkewY to unit -> 
                  let
                     from =
                      case prev of
                        SkewY x _ -> x
                        _ -> 0.0
                  in
                    SkewY (val from to) unit

                Perspective to -> 
                  let
                     from =
                      case prev of
                        SkewY x _ -> x
                        _ -> 0.0
                  in
                    Perspective (val from to)

                Matrix a b c x y z -> 
                    case prev of
                      Matrix aFrom bFrom cFrom xFrom yFrom zFrom -> 
                          Matrix (val aFrom a) (val bFrom b) (val cFrom c) 
                                 (val xFrom x) (val yFrom y) (val zFrom z)
                      _ -> 
                          Matrix (val 0.0 a) (val 0.0 b) (val 0.0 c) 
                                 (val 0.0 x) (val 0.0 y) (val 0.0 z)
                   

                Matrix3d a b c d e f g h i j k l m n o p -> 
                    case prev of
                      Matrix3d a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 o2 p2 -> 
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

                MaxHeight a unit -> renderLength a unit
                MaxWidth a unit  -> renderLength a unit
                MinHeight a unit -> renderLength a unit
                MinWidth a unit  -> renderLength a unit

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


                BorderWidth a unit -> renderLength a unit 
                BorderRadius a unit -> renderLength a unit 
                BorderTopLeftRadius a unit -> renderLength a unit 
                BorderTopRightRadius a unit -> renderLength a unit 
                BorderBottomLeftRadius a unit -> renderLength a unit 
                BorderBottomRightRadius a unit -> renderLength a unit 

                LetterSpacing a unit -> renderLength a unit 
                LineHeight    a unit -> renderLength a unit 

                BackgroundPosition x y unit -> renderLength x unit 
                                     ++ " " ++ renderLength y unit

                TransformOrigin x y z unit -> renderLength x unit 
                                    ++ " " ++ renderLength y unit 
                                    ++ " " ++ renderLength z unit



                Color unit x y z    -> 
                      renderColor unit x y z

                BackgroundColor unit x y z -> 
                       renderColor unit x y z

                BorderColor unit x y z -> 
                       renderColor unit x y z

                ColorA unit x y z a -> 
                      renderAlphaColor unit x y z a

                BackgroundColorA unit x y z a -> 
                      renderAlphaColor unit x y z a

                BorderColorA unit x y z a -> 
                      renderAlphaColor unit x y z a

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
                           


renderColor : ColorFormat -> Float -> Float -> Float -> String
renderColor format x y z =
            let
              renderList xs = "(" ++ (String.concat 
                              <| List.intersperse "," 
                              <| List.map toString xs) ++ ")"

              renderIntList xs = renderList <| List.map round xs
            in
                case format of
                  RGB ->
                    "rgb" ++ renderIntList [x,y,z]

                  HSL ->
                    "hsl(" ++ toString x 
                   ++ "," ++ toString y ++ "%"
                   ++ "," ++ toString z ++ "%"
                   ++ ")"


renderAlphaColor : ColorAlphaFormat -> Float -> Float -> Float -> Float -> String
renderAlphaColor format x y z a =
            let
              renderList xs = "(" ++ (String.concat 
                              <| List.intersperse "," 
                              <| List.map toString xs) ++ ")"

              renderIntList xs = renderList <| List.map round xs
            in
              case format of
                RGBA ->
                   "rgba(" ++ toString (round x) 
                     ++ "," ++ toString (round y) 
                     ++ "," ++ toString (round z) 
                     ++ "," ++ toString a
                     ++ ")"
                      
                HSLA ->
                  "hsl(" ++ toString x 
                  ++ "," ++ toString y ++ "%"
                  ++ "," ++ toString z ++ "%"
                  ++ "," ++ toString a
                  ++ ")"


-- private
propId : StyleProperty a -> String
propId prop =
        case prop of
          Prop name _ unit -> name ++ unit
          Opacity _ -> "opacity"
          Height _ unit -> "height" ++ lenUnit unit
          Width _ unit  -> "width" ++ lenUnit unit
          Left _ unit   -> "left" ++ lenUnit unit
          Right _ unit  -> "right" ++ lenUnit unit
          Bottom _ unit -> "bottom" ++ lenUnit unit
          Top _ unit    -> "top" ++ lenUnit unit

          MaxHeight _ unit -> "max-height" ++ lenUnit unit
          MaxWidth _ unit  -> "max-width" ++ lenUnit unit
          MinHeight _ unit -> "min-height" ++ lenUnit unit
          MinWidth _ unit  -> "min-width" ++ lenUnit unit

          Padding _ unit      -> "padding" ++ lenUnit unit
          PaddingLeft _ unit  -> "padding-left" ++ lenUnit unit
          PaddingRight _ unit -> "padding-right" ++ lenUnit unit
          PaddingTop _ unit   -> "padding-top" ++ lenUnit unit
          PaddingBottom _ unit-> "padding-bottom" ++ lenUnit unit

          Margin _ unit      -> "margin" ++ lenUnit unit
          MarginLeft _ unit  -> "margin-left" ++ lenUnit unit
          MarginRight _ unit -> "margin-right" ++ lenUnit unit
          MarginTop _ unit   -> "margin-top" ++ lenUnit unit
          MarginBottom _ unit-> "margin-bottom" ++ lenUnit unit

          BorderWidth _ unit -> "border-width" ++ lenUnit unit
          BorderRadius _ unit -> "border-radius" ++ lenUnit unit
          BorderTopLeftRadius _ unit -> "border-top-left-radius" ++ lenUnit unit
          BorderTopRightRadius _ unit -> "border-top-right-radius" ++ lenUnit unit
          BorderBottomLeftRadius _ unit -> "border-bottom-left-radius" ++ lenUnit unit
          BorderBottomRightRadius _ unit -> "border-bottom-right-radius" ++ lenUnit unit

          LetterSpacing _ unit -> "letter-spacing" ++ lenUnit unit
          LineHeight _ unit -> "line-height" ++ lenUnit unit

          BackgroundPosition _ _ unit -> "background-position" ++ lenUnit unit

          Color unit _ _ _    -> "color" ++ colorUnit unit
          BackgroundColor unit _ _ _ -> "background-color" ++ colorUnit unit
          BorderColor unit _ _ _ -> "border-color" ++ colorUnit unit

          ColorA unit _ _ _ _ -> "color" ++ colorAUnit unit
          BackgroundColorA unit _ _ _ _ -> "background-color" ++ colorAUnit unit
          BorderColorA unit _ _ _ _ -> "border-color" ++ colorAUnit unit


          TransformOrigin _ _ _ unit -> "transform-origin" ++ lenUnit unit
          Matrix _ _ _ _ _ _ -> "matrix"
          Matrix3d _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ -> "matrix3d"
          Translate _ _ unit -> "translate" ++ lenUnit unit
          Translate3d _ _ _ unit -> "translate3d"  ++ lenUnit unit
          TranslateX _ unit -> "translatex" ++ lenUnit unit
          TranslateY _ unit -> "translatey" ++ lenUnit unit
          Scale _        -> "scale"
          Scale3d _ _ _  -> "scale3d"
          ScaleX _   -> "scalex"
          ScaleY _   -> "scaley"
          ScaleZ _   -> "scalez"
          Rotate _ unit -> "rotate" ++ angleUnit unit
          Rotate3d _ _ _ _ unit -> "rotate3d" ++ angleUnit unit
          RotateX _ unit   -> "rotatex" ++ angleUnit unit
          RotateY _ unit   -> "rotatey" ++ angleUnit unit
          Skew _ _ unit   -> "skew" ++ angleUnit unit
          SkewX _ unit     -> "skewx" ++ angleUnit unit
          SkewY _ unit     -> "skewy" ++ angleUnit unit
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








