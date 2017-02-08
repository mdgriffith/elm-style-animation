module Animation
    exposing
        ( render
        , interrupt
        , queue
        , wait
        , subscription
        , State
        , Msg
        , Step
        , to
        , toWith
        , toWithEach
        , set
        , repeat
        , loop
        , update
        , style
        , styleWith
        , styleWithEach
        , Interpolation
        , spring
        , easing
        , speed
        , Property
        , Length
        , Angle
        , opacity
        , display
        , inline
        , inlineBlock
        , flex
        , inlineFlex
        , listItem
        , block
        , none
        , top
        , left
        , right
        , bottom
        , width
        , height
        , padding
        , paddingLeft
        , paddingRight
        , paddingTop
        , paddingBottom
        , margin
        , marginLeft
        , marginRight
        , marginTop
        , marginBottom
        , color
        , backgroundColor
        , borderColor
        , borderWidth
        , borderLeftWidth
        , borderRightWidth
        , borderTopWidth
        , borderBottomWidth
        , borderRadius
        , borderTopLeftRadius
        , borderTopRightRadius
        , borderBottomLeftRadius
        , borderBottomRightRadius
        , shadow
        , textShadow
        , insetShadow
        , scale
        , scale3d
        , rotate
        , rotate3d
        , transformOrigin
        , translate
        , translate3d
        , viewBox
        , fill
        , stroke
        , strokeWidth
        , stopColor
        , offset
        , x
        , y
        , cx
        , cy
        , radius
        , radiusX
        , radiusY
        , points
        , path
        , PathStep
        , move
        , moveTo
        , line
        , lineTo
        , horizontal
        , horizontalTo
        , vertical
        , verticalTo
        , close
        , CubicCurve
        , QuadraticCurve
        , curve
        , curveTo
        , curve2
        , curve2To
        , arc
        , Arc
        , filterUrl
        , blur
        , brightness
        , contrast
        , grayscale
        , greyscale
        , hueRotate
        , invert
        , saturate
        , sepia
        , dropShadow
        , px
        , percent
        , em
        , rem
        , turn
        , deg
        , grad
        , rad
        , custom
        , custom2
        , customColor
        , attr
        , attr2
        , attr3
        , attr4
        , attrColor
        , exactly
        )

{-| A library for animations.

# Setting up an animation
@docs State, subscription, Msg, render

# Creating an animation
@docs interrupt, queue, Step, wait, to, toWith, toWithEach, set, repeat, loop, update, style, styleWith, styleWithEach, Interpolation, spring, easing, speed

# Animatable Properties
@docs Property, opacity, Length, top, left, right, bottom, width, height, padding, paddingLeft, paddingRight, paddingTop, paddingBottom, margin, marginLeft, marginRight, marginTop, marginBottom, color, backgroundColor, borderColor, borderWidth, borderLeftWidth, borderRightWidth, borderTopWidth, borderBottomWidth, borderRadius, borderTopLeftRadius, borderTopRightRadius, borderBottomLeftRadius, borderBottomRightRadius, shadow, textShadow, insetShadow, display, inline, inlineBlock, flex, inlineFlex, block, none, listItem

# Transforms
@docs scale, scale3d, Angle, rotate, rotate3d, translate, translate3d, transformOrigin

# Animatable CSS Filters
@docs filterUrl, blur, brightness, contrast, grayscale, greyscale, hueRotate, invert, saturate, sepia, dropShadow

# Animatable Svg Properties
@docs viewBox, fill, stroke, strokeWidth, stopColor, offset, x, y, cx, cy, radius, radiusX, radiusY, points

# Constructing an Svg Path
@docs path, PathStep, move, moveTo, line, lineTo, horizontal, horizontalTo, vertical, verticalTo,  close, QuadraticCurve, curve, curveTo, CubicCurve, curve2, curve2To, arc, Arc

# Units
@docs px, percent, em, rem, turn, deg, grad, rad

# Advanced
@docs exactly, custom, custom2, customColor, attr, attr2, attr3, attr4, attrColor

-}

import Html
import Color exposing (Color)
import Time exposing (Time, second)
import AnimationFrame
import Animation.Model exposing (..)
import Animation.Render


{-| _Note_ - The compiler will refer to your `Animation.State` as `Animation.Model.Animation msg`
-}
type alias State =
    Animation.Model.Animation Never


{-| -}
type alias Msg =
    Animation.Model.Tick


{-| -}
type alias Property =
    Animation.Model.Property


{-| -}
type alias Interpolation =
    Animation.Model.Interpolation


{-| -}
type alias PathStep =
    Animation.Model.PathCommand


{-| -}
type alias Step =
    Animation.Model.Step Never



---------------------------
-- Setting Defaults
--------------------------


{-| Specify a custom Spring to animate with.  To be used in conjunction with `StyleWith`, `StyleWithEach`, `toWith`, and `toWithEach`.

This should be your preferred interpolation to use.

-}
spring : { stiffness : Float, damping : Float } -> Animation.Model.Interpolation
spring settings =
    Spring settings


{-| Specify a custom Easing to animate with.  To be used in conjunction with `StyleWith`, `StyleWithEach`, `toWith`, and `toWithEach`.

The [elm-community/easing-functions](https://github.com/elm-community/easing-functions) package has a bunch of useful easing functions!

-}
easing : { duration : Time, ease : Float -> Float } -> Animation.Model.Interpolation
easing { duration, ease } =
    Easing
        { progress = 1
        , duration = duration
        , start = 0
        , ease = ease
        }


{-| Specify a speed to animate with.  To be used in conjunction with `StyleWith`, `StyleWithEach`, `toWith`, and `toWithEach`.

Generally you don't want this.  It's used in the special case of the default interpolation for rotation.

Use `Animation.spring` or `Animation.easing` instead as they are more powerful.

-}
speed : { perSecond : Float } -> Animation.Model.Interpolation
speed speed =
    AtSpeed
        speed


setDefaultInterpolation : Animation.Model.Property -> Animation.Model.Property
setDefaultInterpolation prop =
    let
        interp =
            defaultInterpolationByProperty prop
    in
        mapToMotion (\m -> { m | interpolation = interp }) prop


{-|

-}
defaultInterpolationByProperty : Animation.Model.Property -> Animation.Model.Interpolation
defaultInterpolationByProperty prop =
    let
        spring =
            Spring
                { stiffness = 170
                , damping = 26
                }

        -- progress is set to 1 because it is changed to 0 when the animation actually starts
        -- This is analagous to the spring starting at rest.
        linear duration =
            Easing
                { progress = 1
                , start = 0
                , duration = duration
                , ease = identity
                }
    in
        case prop of
            ExactProperty _ _ ->
                spring

            ColorProperty _ _ _ _ _ ->
                linear (0.4 * second)

            ShadowProperty _ _ _ ->
                spring

            Animation.Model.Property _ _ ->
                spring

            Animation.Model.Property2 _ _ _ ->
                spring

            Animation.Model.Property3 name _ _ _ ->
                if name == "rotate3d" then
                    speed { perSecond = pi }
                else
                    spring

            Animation.Model.Property4 _ _ _ _ _ ->
                spring

            AngleProperty _ _ ->
                speed { perSecond = pi }

            Points _ ->
                spring

            Path _ ->
                spring



--------------------
-- Animation Steps
-------------------


{-| -}
wait : Time -> Animation.Model.Step msg
wait till =
    Wait till


{-| Animate to a set of target values, using the default interpolation.

-}
to : List Animation.Model.Property -> Animation.Model.Step msg
to props =
    To props


{-| Animate to a set of target values. Use a temporary interpolation instead of the default.
The interpolation will revert back to default after this step.
-}
toWith : Animation.Model.Interpolation -> List Animation.Model.Property -> Animation.Model.Step msg
toWith interp props =
    ToWith <|
        List.map
            (mapToMotion (\m -> { m | interpolation = interp }))
            props


{-| Animate to a set of target values. Use a temporary interpolation for each property instead of the default.
The interpolation will revert back to default after this step.
-}
toWithEach : List ( Animation.Model.Interpolation, Animation.Model.Property ) -> Animation.Model.Step msg
toWithEach interpProps =
    ToWith <|
        List.map
            (\( interp, prop ) -> mapToMotion (\m -> { m | interpolation = interp }) prop)
            interpProps



--{-| Animate two properties along a relative curve
---}
--along : List (Float, Float) -> (Property, Animation.Model.Property) -> Step msg


{-| Immediately set properties to a value.
-}
set : List Animation.Model.Property -> Animation.Model.Step msg
set props =
    Set props


{-| Repeat a number of steps `n` times.
-}
repeat : Int -> List (Animation.Model.Step msg) -> Animation.Model.Step msg
repeat n steps =
    Repeat n steps


{-| Repeat a number of steps until interrupted.
-}
loop : List (Animation.Model.Step msg) -> Animation.Model.Step msg
loop steps =
    Loop steps


initialState : List Animation.Model.Property -> Animation msg
initialState current =
    Animation
        { steps = []
        , style = current
        , timing =
            { current = 0
            , dt = 0
            }
        , running = False
        , interruption = []
        }


{-| Set an initial style for an animation.

Uses standard defaults for interpolation

-}
style : List Animation.Model.Property -> Animation msg
style props =
    initialState <| List.map setDefaultInterpolation (Animation.Render.warnForDoubleListedProperties props)


{-| Set an initial style for an animation and override the standard default for interpolation.

-}
styleWith : Animation.Model.Interpolation -> List Animation.Model.Property -> Animation msg
styleWith interp props =
    initialState <| List.map (mapToMotion (\m -> { m | interpolation = interp })) (Animation.Render.warnForDoubleListedProperties props)


{-| Set an initial style for an animation and specify the interpolation to be used for each property.

Any property not listed will receive interpolation based on the standard defaults.
-}
styleWithEach : List ( Animation.Model.Interpolation, Animation.Model.Property ) -> Animation msg
styleWithEach props =
    let
        _ =
            Animation.Render.warnForDoubleListedProperties <| List.map Tuple.second props
    in
        initialState <| List.map (\( interp, prop ) -> mapToMotion (\m -> { m | interpolation = interp }) prop) props


{-| Add an animation to the queue, execiting once the current animation finishes

-}
queue : List (Animation.Model.Step msg) -> Animation msg -> Animation msg
queue steps (Animation model) =
    Animation
        { model
            | steps = model.steps ++ steps
            , running = True
        }


{-| Interrupt any running animations with the following animation.

-}
interrupt : List (Animation.Model.Step msg) -> Animation msg -> Animation msg
interrupt steps (Animation model) =
    Animation
        { model
            | interruption = extractInitialWait steps :: model.interruption
            , running = True
        }


{-| Sums all leading `Wait` steps and removes them from the animation.

This is used because the wait at the start of an interruption works differently than a normal wait.


-}
extractInitialWait : List (Animation.Model.Step msg) -> ( Time, List (Animation.Model.Step msg) )
extractInitialWait steps =
    case List.head steps of
        Nothing ->
            ( 0, [] )

        Just step ->
            case step of
                Wait till ->
                    let
                        ( additionalTime, remainingSteps ) =
                            extractInitialWait (List.drop 1 steps)
                    in
                        ( till + additionalTime, remainingSteps )

                _ ->
                    ( 0, steps )


{-| Create a subscription to AnimationFrame.times.

It is throttled based on whether the current animation is running or not.

-}
subscription : (Msg -> msgB) -> List (Animation msgA) -> Sub msgB
subscription msg states =
    if List.any isRunning states then
        Sub.map msg (AnimationFrame.times Tick)
    else
        Sub.none


isRunning : Animation msg -> Bool
isRunning (Animation model) =
    model.running


{-|
-}
debug : Animation msg -> List ( String, Motion, Time )
debug (Animation model) =
    let
        time =
            model.timing.current

        getValueTuple prop =
            case prop of
                ExactProperty _ _ ->
                    []

                ColorProperty name r g b a ->
                    [ ( name ++ "-red", r, time )
                    , ( name ++ "-green", g, time )
                    , ( name ++ "-blue", b, time )
                    , ( name ++ "-alpha", a, time )
                    ]

                ShadowProperty propName inset shadow ->
                    let
                        name =
                            if inset then
                                propName ++ "-inset"
                            else
                                propName
                    in
                        [ ( name ++ "-offsetX", shadow.offsetX, time )
                        , ( name ++ "-offsetY", shadow.offsetY, time )
                        , ( name ++ "-size", shadow.size, time )
                        , ( name ++ "-blur", shadow.blur, time )
                        , ( name ++ "-red", shadow.red, time )
                        , ( name ++ "-green", shadow.green, time )
                        , ( name ++ "-blue", shadow.blue, time )
                        , ( name ++ "-alpha", shadow.alpha, time )
                        ]

                Property name m1 ->
                    [ ( name, m1, time ) ]

                Property2 name m1 m2 ->
                    [ ( name ++ "-x", m1, time )
                    , ( name ++ "-y", m2, time )
                    ]

                Property3 name m1 m2 m3 ->
                    [ ( name ++ "-x", m1, time )
                    , ( name ++ "-y", m2, time )
                    , ( name ++ "-z", m3, time )
                    ]

                Property4 name m1 m2 m3 m4 ->
                    [ ( name ++ "-w", m1, time )
                    , ( name ++ "-x", m2, time )
                    , ( name ++ "-y", m3, time )
                    , ( name ++ "-z", m4, time )
                    ]

                AngleProperty name m1 ->
                    [ ( name, m1, time ) ]

                Points ms ->
                    let
                        name =
                            "points"
                    in
                        List.concat <|
                            List.indexedMap
                                (\i ( x, y ) ->
                                    [ ( toString i ++ name ++ "-x", x, time )
                                    , ( toString i ++ name ++ "-y", y, time )
                                    ]
                                )
                                ms

                Path cmds ->
                    []
    in
        List.concatMap getValueTuple model.style


{-| Update an animation.
-}
update : Msg -> Animation msg -> Animation msg
update tick animation =
    Tuple.first <| updateAnimation tick animation



------------------------
-- Properties and Units
------------------------


type LengthUnit
    = NoUnit
    | Px
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


lengthUnitName : LengthUnit -> String
lengthUnitName unit =
    case unit of
        NoUnit ->
            ""

        Px ->
            "px"

        Percent ->
            "%"

        Rem ->
            "rem"

        Em ->
            "em"

        Ex ->
            "ex"

        Ch ->
            "ch"

        Vh ->
            "vh"

        Vw ->
            "vw"

        Vmin ->
            "vmin"

        Vmax ->
            "vmax"

        Mm ->
            "mm"

        Cm ->
            "cm"

        In ->
            "in"

        Pt ->
            "pt"

        Pc ->
            "pc"


{-|
-}
type Length
    = Length Float LengthUnit


{-|
-}
type Angle
    = Radians Float


initMotion : Float -> String -> Motion
initMotion position unit =
    { position = position
    , velocity = 0
    , target = position
    , unit = unit
    , interpolation =
        Spring
            { stiffness = 170
            , damping = 26
            }
    , interpolationOverride = Nothing
    }


{-| -}
deg : Float -> Angle
deg a =
    Radians <| (a / 360) * (2 * pi)


{-| -}
grad : Float -> Angle
grad a =
    Radians <| (a / 400) * (2 * pi)


{-| -}
rad : Float -> Angle
rad a =
    Radians a


{-| -}
turn : Float -> Angle
turn a =
    Radians <| a * (2 * pi)


{-| -}
px : Float -> Length
px x =
    Length x Px


{-| -}
percent : Float -> Length
percent x =
    Length x Percent


{-| -}
rem : Float -> Length
rem x =
    Length x Rem


{-| -}
em : Float -> Length
em x =
    Length x Em


{-| -}
ex : Float -> Length
ex x =
    Length x Ex


{-| -}
ch : Float -> Length
ch x =
    Length x Ch


{-| -}
vh : Float -> Length
vh x =
    Length x Vh


{-| -}
vw : Float -> Length
vw x =
    Length x Vw


{-| -}
vmin : Float -> Length
vmin x =
    Length x Vmin


{-| -}
vmax : Float -> Length
vmax x =
    Length x Vmax


{-| -}
mm : Float -> Length
mm x =
    Length x Mm


{-| -}
cm : Float -> Length
cm x =
    Length x Cm


{-| -}
inches : Float -> Length
inches x =
    Length x In


{-| -}
pt : Float -> Length
pt x =
    Length x Pt


{-| -}
pc : Float -> Length
pc x =
    Length x Pc


length : String -> Float -> String -> Animation.Model.Property
length name x unit =
    Animation.Model.Property name (initMotion x unit)


length2 : String -> ( Float, String ) -> ( Float, String ) -> Animation.Model.Property
length2 name ( x, len ) ( x2, len2 ) =
    Animation.Model.Property2 name
        (initMotion x len)
        (initMotion x2 len2)


length3 : String -> ( Float, String ) -> ( Float, String ) -> ( Float, String ) -> Animation.Model.Property
length3 name ( x, len ) ( x2, len2 ) ( x3, len3 ) =
    Animation.Model.Property3 name
        (initMotion x len)
        (initMotion x2 len2)
        (initMotion x3 len3)


length4 : String -> ( Float, String ) -> ( Float, String ) -> ( Float, String ) -> ( Float, String ) -> Animation.Model.Property
length4 name ( x, len ) ( x2, len2 ) ( x3, len3 ) ( x4, len4 ) =
    Animation.Model.Property4 name
        (initMotion x len)
        (initMotion x2 len2)
        (initMotion x3 len3)
        (initMotion x4 len4)


{-| Animate a custom attribute by providing it's name, a float value, and the units it should have.


-}
attr : String -> Float -> String -> Animation.Model.Property
attr name value unit =
    Animation.Model.Property ("attr:" ++ name) (initMotion value unit)


{-|

-}
attr2 : String -> ( Float, String ) -> ( Float, String ) -> Animation.Model.Property
attr2 name value1 value2 =
    length2 ("attr:" ++ name) value1 value2


{-|
-}
attr3 : String -> ( Float, String ) -> ( Float, String ) -> ( Float, String ) -> Animation.Model.Property
attr3 name value1 value2 value3 =
    length3 ("attr:" ++ name) value1 value2 value3


{-|
-}
attr4 : String -> ( Float, String ) -> ( Float, String ) -> ( Float, String ) -> ( Float, String ) -> Animation.Model.Property
attr4 name value1 value2 value3 value4 =
    length4 ("attr:" ++ name) value1 value2 value3 value4


{-|
-}
attrColor : String -> Color -> Animation.Model.Property
attrColor name color =
    let
        { red, green, blue, alpha } =
            Color.toRgb color
    in
        ColorProperty ("attr:" ++ name)
            (initMotion (toFloat red) "")
            (initMotion (toFloat green) "")
            (initMotion (toFloat blue) "")
            (initMotion alpha "")


{-| Animate a custom _style_ property by providing it's name, a float value, and the units it should have.
-}
custom : String -> Float -> String -> Animation.Model.Property
custom name value unit =
    Animation.Model.Property name (initMotion value unit)


{-|
-}
custom2 : String -> ( Float, String ) -> ( Float, String ) -> Animation.Model.Property
custom2 name value unit =
    length2 name value unit


{-|
-}
customColor : String -> Color -> Animation.Model.Property
customColor name color =
    let
        { red, green, blue, alpha } =
            Color.toRgb color
    in
        ColorProperty name
            (initMotion (toFloat red) "")
            (initMotion (toFloat green) "")
            (initMotion (toFloat blue) "")
            (initMotion alpha "")


{-| Set a non-numerical to an exact value.  This is generally only used with `Animation.set`.

For example

```
Animation.set
    [ Animation.exactly "border-style" "dashed"
    ]
```

-}
exactly : String -> String -> Animation.Model.Property
exactly name value =
    ExactProperty name value


{-| -}
opacity : Float -> Animation.Model.Property
opacity x =
    custom "opacity" x ""


{-| -}
display : DisplayMode -> Animation.Model.Property
display mode =
    ExactProperty "display" (displayModeName mode)


{-| A Display value used for the display property.
A display mode is not animated but can be set using Html.Animation.set
-}
type DisplayMode
    = None
    | Inline
    | InlineBlock
    | Block
    | Flex
    | InlineFlex
    | ListItem


displayModeName : DisplayMode -> String
displayModeName mode =
    case mode of
        None ->
            "none"

        Inline ->
            "inline"

        InlineBlock ->
            "inline-block"

        Block ->
            "block"

        Flex ->
            "flex"

        InlineFlex ->
            "inline-flex"

        ListItem ->
            "list-item"


{-| -}
none : DisplayMode
none =
    None


{-| -}
inline : DisplayMode
inline =
    Inline


{-| -}
inlineBlock : DisplayMode
inlineBlock =
    InlineBlock


{-| -}
block : DisplayMode
block =
    Block


{-| -}
flex : DisplayMode
flex =
    Flex


{-| -}
inlineFlex : DisplayMode
inlineFlex =
    InlineFlex


{-| -}
listItem : DisplayMode
listItem =
    ListItem


{-| -}
height : Length -> Animation.Model.Property
height (Length x len) =
    length "height" x (lengthUnitName len)


{-| -}
width : Length -> Animation.Model.Property
width (Length x len) =
    length "width" x (lengthUnitName len)


{-| -}
left : Length -> Animation.Model.Property
left (Length x len) =
    length "left" x (lengthUnitName len)


{-| -}
top : Length -> Animation.Model.Property
top (Length x len) =
    length "top" x (lengthUnitName len)


{-| -}
right : Length -> Animation.Model.Property
right (Length x len) =
    length "right" x (lengthUnitName len)


{-| -}
bottom : Length -> Animation.Model.Property
bottom (Length x len) =
    length "bottom" x (lengthUnitName len)


{-| -}
maxHeight : Length -> Animation.Model.Property
maxHeight (Length x len) =
    length "max-height" x (lengthUnitName len)


{-| -}
maxWidth : Length -> Animation.Model.Property
maxWidth (Length x len) =
    length "max-width" x (lengthUnitName len)


{-| -}
minHeight : Length -> Animation.Model.Property
minHeight (Length x len) =
    length "min-height" x (lengthUnitName len)


{-| -}
minWidth : Length -> Animation.Model.Property
minWidth (Length x len) =
    length "min-width" x (lengthUnitName len)


{-| -}
padding : Length -> Animation.Model.Property
padding (Length x len) =
    length "padding" x (lengthUnitName len)


{-| -}
paddingLeft : Length -> Animation.Model.Property
paddingLeft (Length x len) =
    length "padding-left" x (lengthUnitName len)


{-| -}
paddingRight : Length -> Animation.Model.Property
paddingRight (Length x len) =
    length "padding-right" x (lengthUnitName len)


{-| -}
paddingTop : Length -> Animation.Model.Property
paddingTop (Length x len) =
    length "padding-top" x (lengthUnitName len)


{-| -}
paddingBottom : Length -> Animation.Model.Property
paddingBottom (Length x len) =
    length "padding-bottom" x (lengthUnitName len)


{-| -}
margin : Length -> Animation.Model.Property
margin (Length x len) =
    length "margin" x (lengthUnitName len)


{-| -}
marginLeft : Length -> Animation.Model.Property
marginLeft (Length x len) =
    length "margin-left" x (lengthUnitName len)


{-| -}
marginRight : Length -> Animation.Model.Property
marginRight (Length x len) =
    length "margin-right" x (lengthUnitName len)


{-| -}
marginTop : Length -> Animation.Model.Property
marginTop (Length x len) =
    length "margin-top" x (lengthUnitName len)


{-| -}
marginBottom : Length -> Animation.Model.Property
marginBottom (Length x len) =
    length "margin-bottom" x (lengthUnitName len)


{-| -}
borderWidth : Length -> Animation.Model.Property
borderWidth (Length x len) =
    length "border-width" x (lengthUnitName len)


{-| -}
borderLeftWidth : Length -> Animation.Model.Property
borderLeftWidth (Length x len) =
    length "border-left-width" x (lengthUnitName len)


{-| -}
borderRightWidth : Length -> Animation.Model.Property
borderRightWidth (Length x len) =
    length "border-right-width" x (lengthUnitName len)


{-| -}
borderTopWidth : Length -> Animation.Model.Property
borderTopWidth (Length x len) =
    length "border-top-width" x (lengthUnitName len)


{-| -}
borderBottomWidth : Length -> Animation.Model.Property
borderBottomWidth (Length x len) =
    length "border-bottom-width" x (lengthUnitName len)


{-| -}
borderRadius : Length -> Animation.Model.Property
borderRadius (Length x len) =
    length "border-radius" x (lengthUnitName len)


{-| -}
borderTopLeftRadius : Length -> Animation.Model.Property
borderTopLeftRadius (Length x len) =
    length "border-top-left-radius" x (lengthUnitName len)


{-| -}
borderTopRightRadius : Length -> Animation.Model.Property
borderTopRightRadius (Length x len) =
    length "border-top-right-radius" x (lengthUnitName len)


{-| -}
borderBottomLeftRadius : Length -> Animation.Model.Property
borderBottomLeftRadius (Length x len) =
    length "border-bottom-left-radius" x (lengthUnitName len)


{-| -}
borderBottomRightRadius : Length -> Animation.Model.Property
borderBottomRightRadius (Length x len) =
    length "border-bottom-right-radius" x (lengthUnitName len)


{-| -}
letterSpacing : Length -> Animation.Model.Property
letterSpacing (Length x len) =
    length "letter-spacing" x (lengthUnitName len)


{-| -}
lineHeight : Length -> Animation.Model.Property
lineHeight (Length x len) =
    length "line-height" x (lengthUnitName len)


{-| -}
backgroundPosition : Length -> Length -> Animation.Model.Property
backgroundPosition (Length x len1) (Length y len2) =
    length2 "background-position" ( x, lengthUnitName len1 ) ( y, lengthUnitName len2 )


{-| -}
color : Color -> Animation.Model.Property
color c =
    customColor "color" c


{-| -}
backgroundColor : Color -> Animation.Model.Property
backgroundColor c =
    customColor "background-color" c


{-| -}
borderColor : Color -> Animation.Model.Property
borderColor c =
    customColor "border-color" c


{-| -}
translate : Length -> Length -> Animation.Model.Property
translate (Length x len1) (Length y len2) =
    length2 "translate" ( x, lengthUnitName len1 ) ( y, lengthUnitName len2 )


{-| -}
translate3d : Length -> Length -> Length -> Animation.Model.Property
translate3d (Length x len1) (Length y len2) (Length z len3) =
    length3 "translate3d" ( x, lengthUnitName len1 ) ( y, lengthUnitName len2 ) ( z, lengthUnitName len3 )


{-| -}
transformOrigin : Length -> Length -> Length -> Animation.Model.Property
transformOrigin (Length x len1) (Length y len2) (Length z len3) =
    length3 "transform-origin" ( x, lengthUnitName len1 ) ( y, lengthUnitName len2 ) ( z, lengthUnitName len3 )


{-| -}
scale : Float -> Animation.Model.Property
scale x =
    custom "scale" x ""


{-| -}
scale3d : Float -> Float -> Float -> Animation.Model.Property
scale3d x y z =
    Animation.Model.Property3 "scale3d"
        (initMotion x "")
        (initMotion y "")
        (initMotion z "")


{-| -}
rotate : Angle -> Animation.Model.Property
rotate (Radians x) =
    AngleProperty "rotate" (initMotion x "rad")


{-| -}
rotate3d : Angle -> Angle -> Angle -> Animation.Model.Property
rotate3d (Radians x) (Radians y) (Radians z) =
    length3 "rotate3d" ( x, "rad" ) ( y, "rad" ) ( z, "rad" )


type alias Shadow =
    { offsetX : Float
    , offsetY : Float
    , size : Float
    , blur : Float
    , color : Color
    }


{-| Text shadows will ignore the shadow's `size` value.   This is just one of the bizarre quirks of CSS.
-}
textShadow : Shadow -> Animation.Model.Property
textShadow shade =
    let
        { red, green, blue, alpha } =
            Color.toRgb shade.color
    in
        ShadowProperty
            "text-shadow"
            False
            { offsetX = initMotion shade.offsetX "px"
            , offsetY = initMotion shade.offsetY "px"
            , size = initMotion shade.size "px"
            , blur = initMotion shade.blur "px"
            , red = initMotion (toFloat red) "px"
            , green = initMotion (toFloat green) "px"
            , blue = initMotion (toFloat blue) "px"
            , alpha = initMotion alpha "px"
            }


{-| -}
shadow : Shadow -> Animation.Model.Property
shadow shade =
    let
        { red, green, blue, alpha } =
            Color.toRgb shade.color
    in
        ShadowProperty
            "box-shadow"
            False
            { offsetX = initMotion shade.offsetX "px"
            , offsetY = initMotion shade.offsetY "px"
            , size = initMotion shade.size "px"
            , blur = initMotion shade.blur "px"
            , red = initMotion (toFloat red) "px"
            , green = initMotion (toFloat green) "px"
            , blue = initMotion (toFloat blue) "px"
            , alpha = initMotion alpha "px"
            }


{-| -}
insetShadow : Shadow -> Animation.Model.Property
insetShadow shade =
    let
        { red, green, blue, alpha } =
            Color.toRgb shade.color
    in
        ShadowProperty
            "box-shadow"
            True
            { offsetX = initMotion shade.offsetX "px"
            , offsetY = initMotion shade.offsetY "px"
            , size = initMotion shade.size "px"
            , blur = initMotion shade.blur "px"
            , red = initMotion (toFloat red) "px"
            , green = initMotion (toFloat green) "px"
            , blue = initMotion (toFloat blue) "px"
            , alpha = initMotion alpha "px"
            }



-- SVG properties


{-| -}
x : Float -> Animation.Model.Property
x x =
    custom "x" x ""


{-| -}
y : Float -> Animation.Model.Property
y y =
    custom "y" y ""


{-| -}
cx : Float -> Animation.Model.Property
cx x =
    custom "cx" x ""


{-| -}
cy : Float -> Animation.Model.Property
cy y =
    custom "cy" y ""


{-| -}
radius : Float -> Animation.Model.Property
radius r =
    custom "r" r ""


{-| -}
radiusX : Float -> Animation.Model.Property
radiusX rx =
    custom "rx" rx ""


{-| -}
radiusY : Float -> Animation.Model.Property
radiusY ry =
    custom "ry" ry ""


{-| To be used with the svg path element.  Renders as the d property.
-}
path : List PathCommand -> Animation.Model.Property
path commands =
    Path commands


{-|
-}
move : Float -> Float -> PathCommand
move x y =
    Move (initMotion x "") (initMotion y "")


{-| -}
moveTo : Float -> Float -> PathCommand
moveTo x y =
    MoveTo (initMotion x "") (initMotion y "")


{-| -}
line : Float -> Float -> PathCommand
line x y =
    Line (initMotion x "") (initMotion y "")


{-| -}
lineTo : Float -> Float -> PathCommand
lineTo x y =
    LineTo (initMotion x "") (initMotion y "")


{-| -}
horizontal : Float -> PathCommand
horizontal x =
    Horizontal (initMotion x "")


{-| -}
horizontalTo : Float -> PathCommand
horizontalTo x =
    HorizontalTo (initMotion x "")


{-| -}
vertical : Float -> PathCommand
vertical x =
    Vertical (initMotion x "")


{-| -}
verticalTo : Float -> PathCommand
verticalTo x =
    VerticalTo (initMotion x "")


{-| -}
type alias CubicCurve =
    { control1 : ( Float, Float )
    , control2 : ( Float, Float )
    , point : ( Float, Float )
    }


{-| -}
type alias QuadraticCurve =
    { control : ( Float, Float )
    , point : ( Float, Float )
    }


{-| Create a relative Curve with 2 control points and a target point.
This is a Cubic Curve in the svg spec.

-}
curve2 : CubicCurve -> PathCommand
curve2 { control1, control2, point } =
    Curve
        { control1 =
            ( initMotion (Tuple.first control1) ""
            , initMotion (Tuple.second control1) ""
            )
        , control2 =
            ( initMotion (Tuple.first control2) ""
            , initMotion (Tuple.second control2) ""
            )
        , point =
            ( initMotion (Tuple.first point) ""
            , initMotion (Tuple.second point) ""
            )
        }


{-| Create an absolute Curve with 2 control points and a target point.
This is a Cubic Curve in the svg spec.

-}
curve2To : CubicCurve -> PathCommand
curve2To { control1, control2, point } =
    CurveTo
        { control1 =
            ( initMotion (Tuple.first control1) ""
            , initMotion (Tuple.second control1) ""
            )
        , control2 =
            ( initMotion (Tuple.first control2) ""
            , initMotion (Tuple.second control2) ""
            )
        , point =
            ( initMotion (Tuple.first point) ""
            , initMotion (Tuple.second point) ""
            )
        }


{-| Create a relative curve with 1 control point and a target point.
This is a Quadratic curve in teh svg spec.
-}
curve : QuadraticCurve -> PathCommand
curve { control, point } =
    Quadratic
        { control =
            ( initMotion (Tuple.first control) ""
            , initMotion (Tuple.second control) ""
            )
        , point =
            ( initMotion (Tuple.first point) ""
            , initMotion (Tuple.second point) ""
            )
        }


{-| Create an absolute curve with 1 control point and a target point.
This is a Quadratic curve in teh svg spec.
-}
curveTo : QuadraticCurve -> PathCommand
curveTo { control, point } =
    QuadraticTo
        { control =
            ( initMotion (Tuple.first control) ""
            , initMotion (Tuple.second control) ""
            )
        , point =
            ( initMotion (Tuple.first point) ""
            , initMotion (Tuple.second point) ""
            )
        }


{-| -}
type alias Arc =
    { x : Float
    , y : Float
    , radius : Float
    , startAngle : Float
    , endAngle : Float
    , clockwise : Bool
    }


{-|
-}
arc : Arc -> PathCommand
arc arc =
    if arc.clockwise then
        ClockwiseArc
            { x = initMotion arc.x ""
            , y = initMotion arc.y ""
            , radius = initMotion arc.radius ""
            , startAngle = initMotion arc.startAngle ""
            , endAngle = initMotion arc.endAngle ""
            }
    else
        AntiClockwiseArc
            { x = initMotion arc.x ""
            , y = initMotion arc.y ""
            , radius = initMotion arc.radius ""
            , startAngle = initMotion arc.startAngle ""
            , endAngle = initMotion arc.endAngle ""
            }


{-| Close a Path
-}
close : PathCommand
close =
    Close


{-| Create a CSS filter-url
-}
filterUrl : String -> Animation.Model.Property
filterUrl url =
    exactly "filter-url" url


{-| Create a CSS blur filter, these stack with other filters.
-}
blur : Length -> Animation.Model.Property
blur (Length x len) =
    length "blur" x (lengthUnitName len)


{-| Create a CSS brightness filter, these stack with other filters.
-}
brightness : Float -> Animation.Model.Property
brightness x =
    custom "brightness" x "%"


{-| Create a CSS contrast filter, these stack with other filters.
-}
contrast : Float -> Animation.Model.Property
contrast x =
    custom "contrast" x "%"


{-| Create a CSS grayscale filter, these stack with other filters.
-}
grayscale : Float -> Animation.Model.Property
grayscale x =
    custom "grayscale" x "%"


{-| Create a CSS grayscale filter, these stack with other filters.  This is a spelling adjusment.
-}
greyscale : Float -> Animation.Model.Property
greyscale x =
    grayscale x


{-| Create a CSS hue-rotation filter, these stack with other filters.
-}
hueRotate : Angle -> Animation.Model.Property
hueRotate (Radians x) =
    AngleProperty "hue-rotate" (initMotion x "rad")


{-| Create a CSS invert filter, these stack with other filters.
-}
invert : Float -> Animation.Model.Property
invert x =
    custom "invert" x "%"


{-| Create a CSS saturate filter, these stack with other filters.
-}
saturate : Float -> Animation.Model.Property
saturate x =
    custom "saturate" x "%"


{-| Create a CSS sepia filter, these stack with other filters.
-}
sepia : Float -> Animation.Model.Property
sepia x =
    custom "sepia" x "%"


{-| Drop shadows will ignore the shadow's `size` value.   This is just one of the bizarre quirks of CSS.
-}
dropShadow : Shadow -> Animation.Model.Property
dropShadow shade =
    let
        { red, green, blue, alpha } =
            Color.toRgb shade.color
    in
        ShadowProperty
            "drop-shadow"
            False
            { offsetX = initMotion shade.offsetX "px"
            , offsetY = initMotion shade.offsetY "px"
            , size = initMotion shade.size "px"
            , blur = initMotion shade.blur "px"
            , red = initMotion (toFloat red) "px"
            , green = initMotion (toFloat green) "px"
            , blue = initMotion (toFloat blue) "px"
            , alpha = initMotion alpha "px"
            }


{-| Used with the svg polygon element
-}
points : List ( Float, Float ) -> Animation.Model.Property
points pnts =
    Points <|
        List.map
            (\( x, y ) ->
                ( initMotion x "", initMotion y "" )
            )
            (alignStartingPoint pnts)


{-| -}
viewBox : Float -> Float -> Float -> Float -> Animation.Model.Property
viewBox w x y z =
    length4 "viewBox" ( w, "" ) ( x, "" ) ( y, "" ) ( z, "" )


{-| -}
fill : Color -> Animation.Model.Property
fill color =
    customColor "fill" color


{-| -}
stroke : Color -> Animation.Model.Property
stroke color =
    customColor "stroke" color


{-| -}
strokeWidth : Float -> Animation.Model.Property
strokeWidth x =
    length "stroke-width" x ""


{-| Used for svg gradients
-}
stopColor : Color -> Animation.Model.Property
stopColor color =
    customColor "stop-color" color


{-| Used for svg gradients.  Accepts a number between 0 and 1.
-}
offset : Float -> Animation.Model.Property
offset value =
    custom "offset" value ""


{-| Given two lists of coordinates, rotate the list so that the lowest coordinate is first.

This is to align polygon coordinates so that they can morph smoothely into each other.
-}
alignStartingPoint : List ( Float, Float ) -> List ( Float, Float )
alignStartingPoint points =
    let
        sums =
            List.map (\( x, y ) -> x + y) points

        maybeMin =
            List.minimum sums

        indexOfLowestPoint =
            case maybeMin of
                Nothing ->
                    Nothing

                Just min ->
                    List.head <|
                        List.filterMap identity <|
                            List.indexedMap
                                (\i val ->
                                    if val == min then
                                        Just i
                                    else
                                        Nothing
                                )
                                sums
    in
        case indexOfLowestPoint of
            Nothing ->
                points

            Just i ->
                (List.drop i points) ++ (List.take i points)



-------------------------
-- Rendering
-------------------------


{-| Render style properties into the style attribute and render other attributes as needed for svg.

Combine "transform" based properties into a single css property.

Combine "filter" based properties into a single css property.
-}
render : Animation msgA -> List (Html.Attribute msgB)
render =
    Animation.Render.render
