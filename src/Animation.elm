module Animation
    exposing
        ( Angle
        , Arc
        , Color
        , CubicCurve
        , Interpolation
        , Length
        , Msg
        , PathStep
        , Property
        , QuadraticCurve
        , State
        , Step
        , arc
        , attr
        , attr2
        , attr3
        , attr4
        , attrColor
        , backgroundColor
        , block
        , blur
        , borderBottomLeftRadius
        , borderBottomRightRadius
        , borderBottomWidth
        , borderColor
        , borderLeftWidth
        , borderRadius
        , borderRightWidth
        , borderTopLeftRadius
        , borderTopRightRadius
        , borderTopWidth
        , borderWidth
        , bottom
        , brightness
        , close
        , color
        , contrast
        , curve
        , curve2
        , curve2To
        , curveTo
        , custom
        , custom2
        , customColor
        , cx
        , cy
        , deg
        , display
        , dropShadow
        , easing
        , em
        , exactly
        , fill
        , filterUrl
        , flex
        , grad
        , grayscale
        , greyscale
        , height
        , horizontal
        , horizontalTo
        , hueRotate
        , inline
        , inlineBlock
        , inlineFlex
        , insetShadow
        , interrupt
        , invert
        , left
        , line
        , lineTo
        , listItem
        , loop
        , margin
        , marginBottom
        , marginLeft
        , marginRight
        , marginTop
        , move
        , moveTo
        , none
        , offset
        , opacity
        , padding
        , paddingBottom
        , paddingLeft
        , paddingRight
        , paddingTop
        , path
        , percent
        , points
        , px
        , queue
        , rad
        , radius
        , radiusX
        , radiusY
        , rem
        , render
        , renderPairs
        , repeat
        , right
        , rotate
        , rotate3d
        , saturate
        , scale
        , scale3d
        , sepia
        , set
        , shadow
        , speed
        , spring
        , stopColor
        , stroke
        , strokeWidth
        , style
        , styleWith
        , styleWithEach
        , subscription
        , textShadow
        , to
        , toWith
        , toWithEach
        , top
        , transformOrigin
        , translate
        , translate3d
        , turn
        , update
        , vertical
        , verticalTo
        , viewBox
        , wait
        , width
        , x
        , y
        )

{-| A library for animations.


# Setting up an animation

@docs State, subscription, Msg, render, renderPairs


# Creating an animation

@docs interrupt, queue, Step, wait, to, toWith, toWithEach, set, repeat, loop, update, style, styleWith, styleWithEach, Interpolation, spring, easing, speed


# Animatable Properties

@docs Property, opacity, Length, top, left, right, bottom, width, height, padding, paddingLeft, paddingRight, paddingTop, paddingBottom, margin, marginLeft, marginRight, marginTop, marginBottom, Color, color, backgroundColor, borderColor, borderWidth, borderLeftWidth, borderRightWidth, borderTopWidth, borderBottomWidth, borderRadius, borderTopLeftRadius, borderTopRightRadius, borderBottomLeftRadius, borderBottomRightRadius, shadow, textShadow, insetShadow, display, inline, inlineBlock, flex, inlineFlex, block, none, listItem


# Transforms

@docs scale, scale3d, Angle, rotate, rotate3d, translate, translate3d, transformOrigin


# Animatable CSS Filters

@docs filterUrl, blur, brightness, contrast, grayscale, greyscale, hueRotate, invert, saturate, sepia, dropShadow


# Animatable Svg Properties

@docs viewBox, fill, stroke, strokeWidth, stopColor, offset, x, y, cx, cy, radius, radiusX, radiusY, points


# Constructing an Svg Path

@docs path, PathStep, move, moveTo, line, lineTo, horizontal, horizontalTo, vertical, verticalTo, close, QuadraticCurve, curve, curveTo, CubicCurve, curve2, curve2To, arc, Arc


# Units

@docs px, percent, em, rem, turn, deg, grad, rad


# Advanced

@docs exactly, custom, custom2, customColor, attr, attr2, attr3, attr4, attrColor

-}

import Animation.Model exposing (..)
import Animation.Render
import Browser.Events
import Html
import Time


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


{-| -}
type alias Milliseconds =
    Float


{-| Specify a custom Spring to animate with. To be used in conjunction with `StyleWith`, `StyleWithEach`, `toWith`, and `toWithEach`.

This should be your preferred interpolation to use.

-}
spring : { stiffness : Float, damping : Float } -> Animation.Model.Interpolation
spring settings =
    Spring settings


{-| Specify a custom Easing to animate with. To be used in conjunction with `StyleWith`, `StyleWithEach`, `toWith`, and `toWithEach`.

The [elm-community/easing-functions](https://github.com/elm-community/easing-functions) package has a bunch of useful easing functions!

-}
easing : { duration : Milliseconds, ease : Float -> Float } -> Animation.Model.Interpolation
easing { duration, ease } =
    Easing
        { progress = 1
        , duration = Time.millisToPosix (round duration)
        , start = 0
        , ease = ease
        }


{-| Specify a speed to animate with. To be used in conjunction with `StyleWith`, `StyleWithEach`, `toWith`, and `toWithEach`.

Generally you don't want this. It's used in the special case of the default interpolation for rotation.

Use `Animation.spring` or `Animation.easing` instead as they are more powerful.

-}
speed : { perSecond : Float } -> Animation.Model.Interpolation
speed speedValue =
    AtSpeed
        speedValue


setDefaultInterpolation : Animation.Model.Property -> Animation.Model.Property
setDefaultInterpolation prop =
    let
        interp =
            defaultInterpolationByProperty prop
    in
    mapToMotion (\m -> { m | interpolation = interp }) prop


{-| -}
defaultInterpolationByProperty : Animation.Model.Property -> Animation.Model.Interpolation
defaultInterpolationByProperty prop =
    let
        -- progress is set to 1 because it is changed to 0 when the animation actually starts
        -- This is analagous to the spring starting at rest.
        linear duration =
            Easing
                { progress = 1
                , start = 0
                , duration = duration
                , ease = identity
                }

        defaultSpring =
            Spring
                { stiffness = 170
                , damping = 26
                }
    in
    case prop of
        ExactProperty _ _ ->
            defaultSpring

        ColorProperty _ _ _ _ _ ->
            linear (Time.millisToPosix 400)

        ShadowProperty _ _ _ ->
            defaultSpring

        Animation.Model.Property _ _ ->
            defaultSpring

        Animation.Model.Property2 _ _ _ ->
            defaultSpring

        Animation.Model.Property3 name _ _ _ ->
            if name == "rotate3d" then
                speed { perSecond = pi }
            else
                defaultSpring

        Animation.Model.Property4 _ _ _ _ _ ->
            defaultSpring

        AngleProperty _ _ ->
            speed { perSecond = pi }

        Points _ ->
            defaultSpring

        Path _ ->
            defaultSpring



--------------------
-- Animation Steps
-------------------


{-| -}
wait : Time.Posix -> Animation.Model.Step msg
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
            { current = Time.millisToPosix 0
            , dt = Time.millisToPosix 0
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
extractInitialWait : List (Animation.Model.Step msg) -> ( Time.Posix, List (Animation.Model.Step msg) )
extractInitialWait steps =
    case List.head steps of
        Nothing ->
            ( Time.millisToPosix 0, [] )

        Just step ->
            case step of
                Wait till ->
                    let
                        ( additionalTime, remainingSteps ) =
                            extractInitialWait (List.drop 1 steps)
                    in
                    ( Time.millisToPosix (Time.posixToMillis till + Time.posixToMillis additionalTime), remainingSteps )

                _ ->
                    ( Time.millisToPosix 0, steps )


{-| Create a subscription to `Browser.Events.onAnimationFrame`.

It is throttled based on whether the current animation is running or not.

-}
subscription : (Msg -> msgB) -> List (Animation msgA) -> Sub msgB
subscription msg states =
    if List.any isRunning states then
        Sub.map msg (Browser.Events.onAnimationFrame Tick)
    else
        Sub.none


isRunning : Animation msg -> Bool
isRunning (Animation model) =
    model.running


{-| -}
debug : Animation msg -> List ( String, Motion, Time.Posix )
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

                ShadowProperty propName inset shade ->
                    let
                        name =
                            if inset then
                                propName ++ "-inset"
                            else
                                propName
                    in
                    [ ( name ++ "-offsetX", shade.offsetX, time )
                    , ( name ++ "-offsetY", shade.offsetY, time )
                    , ( name ++ "-size", shade.size, time )
                    , ( name ++ "-blur", shade.blur, time )
                    , ( name ++ "-red", shade.red, time )
                    , ( name ++ "-green", shade.green, time )
                    , ( name ++ "-blue", shade.blue, time )
                    , ( name ++ "-alpha", shade.alpha, time )
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
                            (\i ( x_, y_ ) ->
                                [ ( String.fromInt i ++ name ++ "-x", x_, time )
                                , ( String.fromInt i ++ name ++ "-y", y_, time )
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


{-| -}
type Length
    = Length Float LengthUnit


{-| -}
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
px myPx =
    Length myPx Px


{-| -}
percent : Float -> Length
percent perc =
    Length perc Percent


{-| -}
rem : Float -> Length
rem val =
    Length val Rem


{-| -}
em : Float -> Length
em val =
    Length val Em


{-| -}
ex : Float -> Length
ex val =
    Length val Ex


{-| -}
ch : Float -> Length
ch val =
    Length val Ch


{-| -}
vh : Float -> Length
vh val =
    Length val Vh


{-| -}
vw : Float -> Length
vw val =
    Length val Vw


{-| -}
vmin : Float -> Length
vmin val =
    Length val Vmin


{-| -}
vmax : Float -> Length
vmax val =
    Length val Vmax


{-| -}
mm : Float -> Length
mm val =
    Length val Mm


{-| -}
cm : Float -> Length
cm val =
    Length val Cm


{-| -}
inches : Float -> Length
inches val =
    Length val In


{-| -}
pt : Float -> Length
pt val =
    Length val Pt


{-| -}
pc : Float -> Length
pc val =
    Length val Pc


length : String -> Float -> String -> Animation.Model.Property
length name val unit =
    Animation.Model.Property name (initMotion val unit)


length2 : String -> ( Float, String ) -> ( Float, String ) -> Animation.Model.Property
length2 name ( val, len ) ( val2, len2 ) =
    Animation.Model.Property2 name
        (initMotion val len)
        (initMotion val2 len2)


length3 : String -> ( Float, String ) -> ( Float, String ) -> ( Float, String ) -> Animation.Model.Property
length3 name ( val, len ) ( val2, len2 ) ( val3, len3 ) =
    Animation.Model.Property3 name
        (initMotion val len)
        (initMotion val2 len2)
        (initMotion val3 len3)


length4 : String -> ( Float, String ) -> ( Float, String ) -> ( Float, String ) -> ( Float, String ) -> Animation.Model.Property
length4 name ( val, len ) ( val2, len2 ) ( val3, len3 ) ( val4, len4 ) =
    Animation.Model.Property4 name
        (initMotion val len)
        (initMotion val2 len2)
        (initMotion val3 len3)
        (initMotion val4 len4)


{-| Animate a custom attribute by providing it's name, a float value, and the units it should have.
-}
attr : String -> Float -> String -> Animation.Model.Property
attr name value unit =
    Animation.Model.Property ("attr:" ++ name) (initMotion value unit)


{-| -}
attr2 : String -> ( Float, String ) -> ( Float, String ) -> Animation.Model.Property
attr2 name value1 value2 =
    length2 ("attr:" ++ name) value1 value2


{-| -}
attr3 : String -> ( Float, String ) -> ( Float, String ) -> ( Float, String ) -> Animation.Model.Property
attr3 name value1 value2 value3 =
    length3 ("attr:" ++ name) value1 value2 value3


{-| -}
attr4 : String -> ( Float, String ) -> ( Float, String ) -> ( Float, String ) -> ( Float, String ) -> Animation.Model.Property
attr4 name value1 value2 value3 value4 =
    length4 ("attr:" ++ name) value1 value2 value3 value4


{-| -}
attrColor : String -> Color -> Animation.Model.Property
attrColor name { red, green, blue, alpha } =
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


{-| -}
custom2 : String -> ( Float, String ) -> ( Float, String ) -> Animation.Model.Property
custom2 name value unit =
    length2 name value unit


{-| -}
type alias Color =
    { red : Int
    , green : Int
    , blue : Int
    , alpha : Float
    }


{-| -}
customColor : String -> Color -> Animation.Model.Property
customColor name { red, green, blue, alpha } =
    ColorProperty name
        (initMotion (toFloat red) "")
        (initMotion (toFloat green) "")
        (initMotion (toFloat blue) "")
        (initMotion alpha "")


{-| Set a non-numerical to an exact value. This is generally only used with `Animation.set`.

For example

    Animation.set
        [ Animation.exactly "border-style" "dashed"
        ]

-}
exactly : String -> String -> Animation.Model.Property
exactly name value =
    ExactProperty name value


{-| -}
opacity : Float -> Animation.Model.Property
opacity val =
    custom "opacity" val ""


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
height (Length val len) =
    length "height" val (lengthUnitName len)


{-| -}
width : Length -> Animation.Model.Property
width (Length val len) =
    length "width" val (lengthUnitName len)


{-| -}
left : Length -> Animation.Model.Property
left (Length val len) =
    length "left" val (lengthUnitName len)


{-| -}
top : Length -> Animation.Model.Property
top (Length val len) =
    length "top" val (lengthUnitName len)


{-| -}
right : Length -> Animation.Model.Property
right (Length val len) =
    length "right" val (lengthUnitName len)


{-| -}
bottom : Length -> Animation.Model.Property
bottom (Length val len) =
    length "bottom" val (lengthUnitName len)


{-| -}
maxHeight : Length -> Animation.Model.Property
maxHeight (Length val len) =
    length "max-height" val (lengthUnitName len)


{-| -}
maxWidth : Length -> Animation.Model.Property
maxWidth (Length val len) =
    length "max-width" val (lengthUnitName len)


{-| -}
minHeight : Length -> Animation.Model.Property
minHeight (Length val len) =
    length "min-height" val (lengthUnitName len)


{-| -}
minWidth : Length -> Animation.Model.Property
minWidth (Length val len) =
    length "min-width" val (lengthUnitName len)


{-| -}
padding : Length -> Animation.Model.Property
padding (Length val len) =
    length "padding" val (lengthUnitName len)


{-| -}
paddingLeft : Length -> Animation.Model.Property
paddingLeft (Length val len) =
    length "padding-left" val (lengthUnitName len)


{-| -}
paddingRight : Length -> Animation.Model.Property
paddingRight (Length val len) =
    length "padding-right" val (lengthUnitName len)


{-| -}
paddingTop : Length -> Animation.Model.Property
paddingTop (Length val len) =
    length "padding-top" val (lengthUnitName len)


{-| -}
paddingBottom : Length -> Animation.Model.Property
paddingBottom (Length val len) =
    length "padding-bottom" val (lengthUnitName len)


{-| -}
margin : Length -> Animation.Model.Property
margin (Length val len) =
    length "margin" val (lengthUnitName len)


{-| -}
marginLeft : Length -> Animation.Model.Property
marginLeft (Length val len) =
    length "margin-left" val (lengthUnitName len)


{-| -}
marginRight : Length -> Animation.Model.Property
marginRight (Length val len) =
    length "margin-right" val (lengthUnitName len)


{-| -}
marginTop : Length -> Animation.Model.Property
marginTop (Length val len) =
    length "margin-top" val (lengthUnitName len)


{-| -}
marginBottom : Length -> Animation.Model.Property
marginBottom (Length val len) =
    length "margin-bottom" val (lengthUnitName len)


{-| -}
borderWidth : Length -> Animation.Model.Property
borderWidth (Length val len) =
    length "border-width" val (lengthUnitName len)


{-| -}
borderLeftWidth : Length -> Animation.Model.Property
borderLeftWidth (Length val len) =
    length "border-left-width" val (lengthUnitName len)


{-| -}
borderRightWidth : Length -> Animation.Model.Property
borderRightWidth (Length val len) =
    length "border-right-width" val (lengthUnitName len)


{-| -}
borderTopWidth : Length -> Animation.Model.Property
borderTopWidth (Length val len) =
    length "border-top-width" val (lengthUnitName len)


{-| -}
borderBottomWidth : Length -> Animation.Model.Property
borderBottomWidth (Length val len) =
    length "border-bottom-width" val (lengthUnitName len)


{-| -}
borderRadius : Length -> Animation.Model.Property
borderRadius (Length val len) =
    length "border-radius" val (lengthUnitName len)


{-| -}
borderTopLeftRadius : Length -> Animation.Model.Property
borderTopLeftRadius (Length val len) =
    length "border-top-left-radius" val (lengthUnitName len)


{-| -}
borderTopRightRadius : Length -> Animation.Model.Property
borderTopRightRadius (Length val len) =
    length "border-top-right-radius" val (lengthUnitName len)


{-| -}
borderBottomLeftRadius : Length -> Animation.Model.Property
borderBottomLeftRadius (Length val len) =
    length "border-bottom-left-radius" val (lengthUnitName len)


{-| -}
borderBottomRightRadius : Length -> Animation.Model.Property
borderBottomRightRadius (Length val len) =
    length "border-bottom-right-radius" val (lengthUnitName len)


{-| -}
letterSpacing : Length -> Animation.Model.Property
letterSpacing (Length val len) =
    length "letter-spacing" val (lengthUnitName len)


{-| -}
lineHeight : Length -> Animation.Model.Property
lineHeight (Length val len) =
    length "line-height" val (lengthUnitName len)


{-| -}
backgroundPosition : Length -> Length -> Animation.Model.Property
backgroundPosition (Length val len1) (Length valY len2) =
    length2 "background-position" ( val, lengthUnitName len1 ) ( valY, lengthUnitName len2 )


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
translate (Length valX len1) (Length valY len2) =
    length2 "translate" ( valX, lengthUnitName len1 ) ( valY, lengthUnitName len2 )


{-| -}
translate3d : Length -> Length -> Length -> Animation.Model.Property
translate3d (Length valX len1) (Length valY len2) (Length valZ len3) =
    length3 "translate3d" ( valX, lengthUnitName len1 ) ( valY, lengthUnitName len2 ) ( valZ, lengthUnitName len3 )


{-| -}
transformOrigin : Length -> Length -> Length -> Animation.Model.Property
transformOrigin (Length valX len1) (Length valY len2) (Length valZ len3) =
    length3 "transform-origin" ( valX, lengthUnitName len1 ) ( valY, lengthUnitName len2 ) ( valZ, lengthUnitName len3 )


{-| -}
scale : Float -> Animation.Model.Property
scale valX =
    custom "scale" valX ""


{-| -}
scale3d : Float -> Float -> Float -> Animation.Model.Property
scale3d valX valY valZ =
    Animation.Model.Property3 "scale3d"
        (initMotion valX "")
        (initMotion valY "")
        (initMotion valZ "")


{-| -}
rotate : Angle -> Animation.Model.Property
rotate (Radians valX) =
    AngleProperty "rotate" (initMotion valX "rad")


{-| -}
rotate3d : Angle -> Angle -> Angle -> Animation.Model.Property
rotate3d (Radians valX) (Radians valY) (Radians valZ) =
    length3 "rotate3d" ( valX, "rad" ) ( valY, "rad" ) ( valZ, "rad" )


type alias Shadow =
    { offsetX : Float
    , offsetY : Float
    , size : Float
    , blur : Float
    , color : Color
    }


{-| Text shadows will ignore the shadow's `size` value. This is just one of the bizarre quirks of CSS.
-}
textShadow : Shadow -> Animation.Model.Property
textShadow shade =
    ShadowProperty
        "text-shadow"
        False
        { offsetX = initMotion shade.offsetX "px"
        , offsetY = initMotion shade.offsetY "px"
        , size = initMotion shade.size "px"
        , blur = initMotion shade.blur "px"
        , red = initMotion (toFloat shade.color.red) "px"
        , green = initMotion (toFloat shade.color.green) "px"
        , blue = initMotion (toFloat shade.color.blue) "px"
        , alpha = initMotion shade.color.alpha "px"
        }


{-| -}
shadow : Shadow -> Animation.Model.Property
shadow shade =
    ShadowProperty
        "box-shadow"
        False
        { offsetX = initMotion shade.offsetX "px"
        , offsetY = initMotion shade.offsetY "px"
        , size = initMotion shade.size "px"
        , blur = initMotion shade.blur "px"
        , red = initMotion (toFloat shade.color.red) "px"
        , green = initMotion (toFloat shade.color.green) "px"
        , blue = initMotion (toFloat shade.color.blue) "px"
        , alpha = initMotion shade.color.alpha "px"
        }


{-| -}
insetShadow : Shadow -> Animation.Model.Property
insetShadow shade =
    ShadowProperty
        "box-shadow"
        True
        { offsetX = initMotion shade.offsetX "px"
        , offsetY = initMotion shade.offsetY "px"
        , size = initMotion shade.size "px"
        , blur = initMotion shade.blur "px"
        , red = initMotion (toFloat shade.color.red) "px"
        , green = initMotion (toFloat shade.color.green) "px"
        , blue = initMotion (toFloat shade.color.blue) "px"
        , alpha = initMotion shade.color.alpha "px"
        }



-- SVG properties


{-| -}
x : Float -> Animation.Model.Property
x x_ =
    custom "x" x_ ""


{-| -}
y : Float -> Animation.Model.Property
y y_ =
    custom "y" y_ ""


{-| -}
cx : Float -> Animation.Model.Property
cx x_ =
    custom "cx" x_ ""


{-| -}
cy : Float -> Animation.Model.Property
cy y_ =
    custom "cy" y_ ""


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


{-| To be used with the svg path element. Renders as the d property.
-}
path : List PathCommand -> Animation.Model.Property
path commands =
    Path commands


{-| -}
move : Float -> Float -> PathCommand
move x_ y_ =
    Move (initMotion x_ "") (initMotion y_ "")


{-| -}
moveTo : Float -> Float -> PathCommand
moveTo x_ y_ =
    MoveTo (initMotion x_ "") (initMotion y_ "")


{-| -}
line : Float -> Float -> PathCommand
line x_ y_ =
    Line (initMotion x_ "") (initMotion y_ "")


{-| -}
lineTo : Float -> Float -> PathCommand
lineTo x_ y_ =
    LineTo (initMotion x_ "") (initMotion y_ "")


{-| -}
horizontal : Float -> PathCommand
horizontal x_ =
    Horizontal (initMotion x_ "")


{-| -}
horizontalTo : Float -> PathCommand
horizontalTo x_ =
    HorizontalTo (initMotion x_ "")


{-| -}
vertical : Float -> PathCommand
vertical x_ =
    Vertical (initMotion x_ "")


{-| -}
verticalTo : Float -> PathCommand
verticalTo x_ =
    VerticalTo (initMotion x_ "")


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


{-| -}
arc : Arc -> PathCommand
arc myArc =
    if myArc.clockwise then
        ClockwiseArc
            { x = initMotion myArc.x ""
            , y = initMotion myArc.y ""
            , radius = initMotion myArc.radius ""
            , startAngle = initMotion myArc.startAngle ""
            , endAngle = initMotion myArc.endAngle ""
            }
    else
        AntiClockwiseArc
            { x = initMotion myArc.x ""
            , y = initMotion myArc.y ""
            , radius = initMotion myArc.radius ""
            , startAngle = initMotion myArc.startAngle ""
            , endAngle = initMotion myArc.endAngle ""
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
blur (Length val len) =
    length "blur" val (lengthUnitName len)


{-| Create a CSS brightness filter, these stack with other filters.
-}
brightness : Float -> Animation.Model.Property
brightness val =
    custom "brightness" val "%"


{-| Create a CSS contrast filter, these stack with other filters.
-}
contrast : Float -> Animation.Model.Property
contrast val =
    custom "contrast" val "%"


{-| Create a CSS grayscale filter, these stack with other filters.
-}
grayscale : Float -> Animation.Model.Property
grayscale val =
    custom "grayscale" val "%"


{-| Create a CSS grayscale filter, these stack with other filters. This is a spelling adjusment.
-}
greyscale : Float -> Animation.Model.Property
greyscale val =
    grayscale val


{-| Create a CSS hue-rotation filter, these stack with other filters.
-}
hueRotate : Angle -> Animation.Model.Property
hueRotate (Radians val) =
    AngleProperty "hue-rotate" (initMotion val "rad")


{-| Create a CSS invert filter, these stack with other filters.
-}
invert : Float -> Animation.Model.Property
invert val =
    custom "invert" val "%"


{-| Create a CSS saturate filter, these stack with other filters.
-}
saturate : Float -> Animation.Model.Property
saturate val =
    custom "saturate" val "%"


{-| Create a CSS sepia filter, these stack with other filters.
-}
sepia : Float -> Animation.Model.Property
sepia val =
    custom "sepia" val "%"


{-| Drop shadows will ignore the shadow's `size` value. This is just one of the bizarre quirks of CSS.
-}
dropShadow : Shadow -> Animation.Model.Property
dropShadow shade =
    ShadowProperty
        "drop-shadow"
        False
        { offsetX = initMotion shade.offsetX "px"
        , offsetY = initMotion shade.offsetY "px"
        , size = initMotion shade.size "px"
        , blur = initMotion shade.blur "px"
        , red = initMotion (toFloat shade.color.red) "px"
        , green = initMotion (toFloat shade.color.green) "px"
        , blue = initMotion (toFloat shade.color.blue) "px"
        , alpha = initMotion shade.color.alpha "px"
        }


{-| Used with the svg polygon element
-}
points : List ( Float, Float ) -> Animation.Model.Property
points pnts =
    Points <|
        List.map
            (\( x_, y_ ) ->
                ( initMotion x_ "", initMotion y_ "" )
            )
            (alignStartingPoint pnts)


{-| -}
viewBox : Float -> Float -> Float -> Float -> Animation.Model.Property
viewBox w_ x_ y_ z_ =
    length4 "viewBox" ( w_, "" ) ( x_, "" ) ( y_, "" ) ( z_, "" )


{-| -}
fill : Color -> Animation.Model.Property
fill clr =
    customColor "fill" clr


{-| -}
stroke : Color -> Animation.Model.Property
stroke clr =
    customColor "stroke" clr


{-| -}
strokeWidth : Float -> Animation.Model.Property
strokeWidth x_ =
    length "stroke-width" x_ ""


{-| Used for svg gradients
-}
stopColor : Color -> Animation.Model.Property
stopColor clr =
    customColor "stop-color" clr


{-| Used for svg gradients. Accepts a number between 0 and 1.
-}
offset : Float -> Animation.Model.Property
offset value =
    custom "offset" value ""


{-| Given two lists of coordinates, rotate the list so that the lowest coordinate is first.

This is to align polygon coordinates so that they can morph smoothely into each other.

-}
alignStartingPoint : List ( Float, Float ) -> List ( Float, Float )
alignStartingPoint pnts =
    let
        sums =
            List.map (\( x_, y_ ) -> x_ + y_) pnts

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
            pnts

        Just i ->
            List.drop i pnts ++ List.take i pnts



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


{-| Render style properties into the style attribute.

Combine "transform" based properties into a single css property.

Combine "filter" based properties into a single css property.

_Note_ this method will _not_ render svg properties like `Animation.points`.

-}
renderPairs : Animation msgA -> List ( String, String )
renderPairs anim =
    anim
        |> Animation.Render.renderValues
        |> Tuple.first
