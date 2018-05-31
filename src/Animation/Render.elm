module Animation.Render exposing (..)

{-| The actual internals of rendering
-}

import Animation.Model exposing (..)
import Html
import Html.Attributes
import Svg.Attributes


renderValues : Animation msgA -> ( List ( String, String ), List Property )
renderValues (Animation model) =
    let
        ( attrProps, styleProps ) =
            List.partition isAttr model.style

        ( style, transforms, filters ) =
            List.foldl
                (\prop ( myStyle, myTransforms, myFilters ) ->
                    if isTransformation prop then
                        ( myStyle
                        , myTransforms ++ [ prop ]
                        , myFilters
                        )
                    else if isFilter prop then
                        ( myStyle
                        , myTransforms
                        , myFilters ++ [ prop ]
                        )
                    else
                        ( myStyle ++ [ prop ]
                        , myTransforms
                        , myFilters
                        )
                )
                ( [], [], [] )
                styleProps

        renderedStyle =
            List.map (\prop -> ( propertyName prop, propertyValue prop " " )) style

        renderedTransforms =
            if List.isEmpty transforms then
                []
            else
                [ ( "transform"
                  , String.join " " <|
                        List.map
                            (\prop ->
                                if propertyName prop == "rotate3d" then
                                    render3dRotation prop
                                else
                                    propertyName prop ++ "(" ++ propertyValue prop ", " ++ ")"
                            )
                            transforms
                  )
                ]

        renderedFilters =
            if List.isEmpty filters then
                []
            else
                [ ( "filter"
                  , String.join " " <|
                        List.map
                            (\prop ->
                                let
                                    name =
                                        propertyName prop
                                in
                                if name == "filter-url" then
                                    "url(\"" ++ propertyValue prop ", " ++ "\")"
                                else
                                    propertyName prop ++ "(" ++ propertyValue prop ", " ++ ")"
                            )
                            filters
                  )
                ]
    in
    ( renderedTransforms ++ renderedFilters ++ renderedStyle
    , attrProps
    )


render : Animation msgA -> List (Html.Attribute msgB)
render animation =
    let
        ( style, attrProps ) =
            renderValues animation

        styleAttr =
            style
                |> List.concatMap prefix
                |> List.map (\( prop, val ) -> Html.Attributes.style prop val)

        otherAttrs =
            List.filterMap renderAttrs attrProps
    in
    styleAttr ++ otherAttrs


renderAttrs : Animation.Model.Property -> Maybe (Html.Attribute msg)
renderAttrs prop =
    if String.startsWith "attr:" (propertyName prop) then
        Just <| Html.Attributes.attribute (String.dropLeft 5 <| propertyName prop) (propertyValue prop " ")
    else
        case prop of
            Points pts ->
                Just <| Svg.Attributes.points <| propertyValue prop " "

            Path cmds ->
                Just <| Svg.Attributes.d <| propertyValue prop " "

            Property name m1 ->
                case name of
                    "x" ->
                        Just <| Svg.Attributes.x <| propertyValue prop " "

                    "y" ->
                        Just <| Svg.Attributes.y <| propertyValue prop " "

                    "cx" ->
                        Just <| Svg.Attributes.cx <| propertyValue prop " "

                    "cy" ->
                        Just <| Svg.Attributes.cy <| propertyValue prop " "

                    "rx" ->
                        Just <| Svg.Attributes.rx <| propertyValue prop " "

                    "ry" ->
                        Just <| Svg.Attributes.ry <| propertyValue prop " "

                    "r" ->
                        Just <| Svg.Attributes.r <| propertyValue prop " "

                    "offset" ->
                        Just <| Svg.Attributes.offset <| propertyValue prop " "

                    _ ->
                        Nothing

            Property4 name m1 m2 m3 m4 ->
                if name == "viewBox" then
                    Just <| Svg.Attributes.viewBox <| propertyValue prop " "
                else
                    Nothing

            _ ->
                Nothing


isTransformation : Animation.Model.Property -> Bool
isTransformation prop =
    List.member (propertyName prop)
        [ "rotate"
        , "rotateX"
        , "rotateY"
        , "rotateZ"
        , "rotate3d"
        , "translate"
        , "translate3d"
        , "scale"
        , "scale3d"
        ]


render3dRotation : Animation.Model.Property -> String
render3dRotation prop =
    case prop of
        Animation.Model.Property3 _ x y z ->
            "rotateX("
                ++ String.fromFloat x.position
                ++ x.unit
                ++ ") rotateY("
                ++ String.fromFloat y.position
                ++ y.unit
                ++ ") rotateZ("
                ++ String.fromFloat z.position
                ++ z.unit
                ++ ")"

        _ ->
            ""


isFilter : Animation.Model.Property -> Bool
isFilter prop =
    List.member (propertyName prop)
        [ "filter-url"
        , "blur"
        , "brightness"
        , "contrast"
        , "grayscale"
        , "hue-rotate"
        , "invert"
        , "saturate"
        , "sepia"
        , "drop-shadow"
        ]


iePrefix : String
iePrefix =
    "-ms-"


webkitPrefix : String
webkitPrefix =
    "-webkit-"


{-| Add a prefix to a name/value pair, if needed.
-}
prefix : ( String, String ) -> List ( String, String )
prefix stylePair =
    let
        propName =
            Tuple.first stylePair

        propValue =
            Tuple.second stylePair
    in
    case propName of
        "transform" ->
            [ stylePair
            , ( iePrefix ++ propName, propValue )
            , ( webkitPrefix ++ propName, propValue )
            ]

        "transform-origin" ->
            [ stylePair
            , ( iePrefix ++ propName, propValue )
            , ( webkitPrefix ++ propName, propValue )
            ]

        "filter" ->
            [ stylePair
            , ( iePrefix ++ propName, propValue )
            , ( webkitPrefix ++ propName, propValue )
            ]

        _ ->
            [ stylePair ]


{-| This property can only be represented as an html attribute
-}
isAttr : Animation.Model.Property -> Bool
isAttr prop =
    String.startsWith "attr:" (propertyName prop)
        || (case prop of
                Points _ ->
                    True

                Path _ ->
                    True

                Property name _ ->
                    (name == "cx")
                        || (name == "cy")
                        || (name == "x")
                        || (name == "y")
                        || (name == "rx")
                        || (name == "ry")
                        || (name == "r")
                        || (name == "offset")

                Property4 name _ _ _ _ ->
                    name == "viewBox"

                _ ->
                    False
           )


propertyValue : Animation.Model.Property -> String -> String
propertyValue prop delim =
    case prop of
        ExactProperty _ value ->
            value

        ColorProperty _ r g b a ->
            "rgba("
                ++ String.fromInt (round r.position)
                ++ ","
                ++ String.fromInt (round g.position)
                ++ ","
                ++ String.fromInt (round b.position)
                ++ ","
                ++ String.fromFloat a.position
                ++ ")"

        ShadowProperty name inset shadow ->
            (if inset then
                "inset "
             else
                ""
            )
                ++ String.fromFloat shadow.offsetX.position
                ++ "px"
                ++ " "
                ++ String.fromFloat shadow.offsetY.position
                ++ "px"
                ++ " "
                ++ String.fromFloat shadow.blur.position
                ++ "px"
                ++ " "
                ++ (if name == "text-shadow" || name == "drop-shadow" then
                        ""
                    else
                        String.fromFloat shadow.size.position
                            ++ "px"
                            ++ " "
                   )
                ++ "rgba("
                ++ String.fromInt (round shadow.red.position)
                ++ ", "
                ++ String.fromInt (round shadow.green.position)
                ++ ", "
                ++ String.fromInt (round shadow.blue.position)
                ++ ", "
                ++ String.fromFloat shadow.alpha.position
                ++ ")"

        Property _ x ->
            String.fromFloat x.position ++ x.unit

        Property2 _ x y ->
            String.fromFloat x.position
                ++ x.unit
                ++ delim
                ++ String.fromFloat y.position
                ++ y.unit

        Property3 _ x y z ->
            String.fromFloat x.position
                ++ x.unit
                ++ delim
                ++ String.fromFloat y.position
                ++ y.unit
                ++ delim
                ++ String.fromFloat z.position
                ++ z.unit

        Property4 _ w x y z ->
            String.fromFloat w.position
                ++ w.unit
                ++ delim
                ++ String.fromFloat x.position
                ++ x.unit
                ++ delim
                ++ String.fromFloat y.position
                ++ y.unit
                ++ delim
                ++ String.fromFloat z.position
                ++ z.unit

        AngleProperty _ x ->
            String.fromFloat x.position ++ x.unit

        Points coords ->
            String.join " " <|
                List.map
                    (\( x, y ) ->
                        String.fromFloat x.position ++ "," ++ String.fromFloat y.position
                    )
                    coords

        Path cmds ->
            String.join " " <|
                List.map pathCmdValue cmds


pathCmdValue : PathCommand -> String
pathCmdValue cmd =
    let
        renderPoints coords =
            String.join " " <|
                List.map
                    (\( x, y ) ->
                        String.fromFloat x.position ++ "," ++ String.fromFloat y.position
                    )
                    coords
    in
    case cmd of
        Move x y ->
            "m " ++ String.fromFloat x.position ++ "," ++ String.fromFloat y.position

        MoveTo x y ->
            "M " ++ String.fromFloat x.position ++ "," ++ String.fromFloat y.position

        Line x y ->
            "l " ++ String.fromFloat x.position ++ "," ++ String.fromFloat y.position

        LineTo x y ->
            "L " ++ String.fromFloat x.position ++ "," ++ String.fromFloat y.position

        Horizontal a ->
            "h " ++ String.fromFloat a.position

        HorizontalTo a ->
            "H " ++ String.fromFloat a.position

        Vertical a ->
            "v " ++ String.fromFloat a.position

        VerticalTo a ->
            "V " ++ String.fromFloat a.position

        Curve { control1, control2, point } ->
            let
                ( c1x, c1y ) =
                    control1

                ( c2x, c2y ) =
                    control2

                ( p1x, p1y ) =
                    point
            in
            "c "
                ++ (String.fromFloat <| c1x.position)
                ++ " "
                ++ (String.fromFloat <| c1y.position)
                ++ ", "
                ++ (String.fromFloat <| c2x.position)
                ++ " "
                ++ (String.fromFloat <| c2y.position)
                ++ ", "
                ++ (String.fromFloat <| p1x.position)
                ++ " "
                ++ (String.fromFloat <| p1y.position)

        CurveTo { control1, control2, point } ->
            let
                ( c1x, c1y ) =
                    control1

                ( c2x, c2y ) =
                    control2

                ( p1x, p1y ) =
                    point
            in
            "C "
                ++ (String.fromFloat <| c1x.position)
                ++ " "
                ++ (String.fromFloat <| c1y.position)
                ++ ", "
                ++ (String.fromFloat <| c2x.position)
                ++ " "
                ++ (String.fromFloat <| c2y.position)
                ++ ", "
                ++ (String.fromFloat <| p1x.position)
                ++ " "
                ++ (String.fromFloat <| p1y.position)

        Quadratic { control, point } ->
            let
                ( c1x, c1y ) =
                    control

                ( p1x, p1y ) =
                    point
            in
            "q "
                ++ (String.fromFloat <| c1x.position)
                ++ " "
                ++ (String.fromFloat <| c1y.position)
                ++ ", "
                ++ (String.fromFloat <| p1x.position)
                ++ " "
                ++ (String.fromFloat <| p1y.position)

        QuadraticTo { control, point } ->
            let
                ( c1x, c1y ) =
                    control

                ( p1x, p1y ) =
                    point
            in
            "Q "
                ++ (String.fromFloat <| c1x.position)
                ++ " "
                ++ (String.fromFloat <| c1y.position)
                ++ ", "
                ++ (String.fromFloat <| p1x.position)
                ++ " "
                ++ (String.fromFloat <| p1y.position)

        SmoothQuadratic points ->
            "t " ++ renderPoints points

        SmoothQuadraticTo points ->
            "T " ++ renderPoints points

        Smooth points ->
            "s " ++ renderPoints points

        SmoothTo points ->
            "S " ++ renderPoints points

        ClockwiseArc arc ->
            let
                deltaAngle =
                    arc.endAngle.position - arc.startAngle.position
            in
            if deltaAngle > (360 - 1.0e-6) then
                let
                    dx =
                        arc.radius.position * cos (degrees arc.startAngle.position)

                    dy =
                        arc.radius.position * sin (degrees arc.startAngle.position)
                in
                "A "
                    ++ String.fromFloat arc.radius.position
                    ++ ","
                    ++ String.fromFloat arc.radius.position
                    ++ ",0,1,1,"
                    ++ String.fromFloat (arc.x.position - dx)
                    ++ ","
                    ++ String.fromFloat (arc.y.position - dy)
                    ++ " A "
                    ++ String.fromFloat arc.radius.position
                    ++ ","
                    ++ String.fromFloat arc.radius.position
                    ++ ",0,1,1,"
                    ++ String.fromFloat (arc.x.position + dx)
                    ++ ","
                    ++ String.fromFloat (arc.y.position + dy)
            else
                "A "
                    ++ String.fromFloat arc.radius.position
                    ++ ","
                    ++ String.fromFloat arc.radius.position
                    ++ " 0 "
                    ++ (if deltaAngle >= 180 then
                            "1"
                        else
                            "0"
                       )
                    ++ " "
                    ++ "1"
                    ++ " "
                    ++ String.fromFloat (arc.x.position + (arc.radius.position * (cos <| degrees arc.endAngle.position)))
                    ++ ","
                    ++ String.fromFloat (arc.y.position + (arc.radius.position * (sin <| degrees arc.endAngle.position)))

        AntiClockwiseArc arc ->
            let
                deltaAngle =
                    arc.endAngle.position - arc.startAngle.position
            in
            if deltaAngle > (360 - 1.0e-6) then
                let
                    dx =
                        arc.radius.position * cos (degrees arc.startAngle.position)

                    dy =
                        arc.radius.position * sin (degrees arc.startAngle.position)
                in
                "A "
                    ++ String.fromFloat arc.radius.position
                    ++ ","
                    ++ String.fromFloat arc.radius.position
                    ++ ",0,1,0,"
                    ++ String.fromFloat (arc.x.position - dx)
                    ++ ","
                    ++ String.fromFloat (arc.y.position - dy)
                    ++ " A "
                    ++ String.fromFloat arc.radius.position
                    ++ ","
                    ++ String.fromFloat arc.radius.position
                    ++ ",0,1,1,"
                    ++ String.fromFloat (arc.x.position + dx)
                    ++ ","
                    ++ String.fromFloat (arc.y.position + dy)
            else
                "A "
                    ++ String.fromFloat arc.radius.position
                    ++ ","
                    ++ String.fromFloat arc.radius.position
                    ++ " 0 "
                    ++ (if arc.startAngle.position - arc.endAngle.position >= 180 then
                            "1"
                        else
                            "0"
                       )
                    ++ " "
                    ++ "0"
                    ++ " "
                    ++ String.fromFloat (arc.x.position + (arc.radius.position * cos arc.endAngle.position))
                    ++ ","
                    ++ String.fromFloat (arc.y.position + (arc.radius.position * sin arc.endAngle.position))

        Close ->
            "z"


warnForDoubleListedProperties : List Animation.Model.Property -> List Animation.Model.Property
warnForDoubleListedProperties props =
    let
        _ =
            List.filter (\prop -> not <| isTransformation prop) props
                |> List.map propertyName
                |> List.sort
                |> groupWhile (==)
                |> List.map
                    (\propGroup ->
                        case List.head propGroup of
                            Nothing ->
                                ""

                            Just name ->
                                if List.length propGroup > 1 then
                                    ""
                                    -- Debug.log "elm-style-animation" ("The \"" ++ name ++ "\" css property is listed more than once.  Only the last instance will be used.")
                                else
                                    ""
                    )
    in
    props


{-| The following functions are copied from elm-community/list-extra.

They were copied because there were version number issues
when the version this library was using and the version
the user was using got out of sync.

Group elements together, using a custom equality test.
groupWhile (\x y -> first x == first y) [(0,'a'),(0,'b'),(1,'c'),(1,'d')] == [[(0,'a'),(0,'b')],[(1,'c'),(1,'d')]]
The equality test should be an equivalent relationship, i.e. it should have the properties of reflexivity, symmetry, and transitivity. For non-equivalent relations it gives non-intuitive behavior:
groupWhile (<) [1,2,3,2,4,1,3,2,1] == [[1,2,3,2,4],[1,3,2],[1]]
For grouping elements with a comparison test, which must only hold the property of transitivity, see `groupWhileTransitively`.

-}
groupWhile : (a -> a -> Bool) -> List a -> List (List a)
groupWhile eq xs_ =
    case xs_ of
        [] ->
            []

        x :: xs ->
            let
                ( ys, zs ) =
                    span (eq x) xs
            in
            (x :: ys) :: groupWhile eq zs


{-| Take a predicate and a list, return a tuple. The first part of the tuple is the longest prefix of that list, for each element of which the predicate holds. The second part of the tuple is the remainder of the list. `span p xs` is equivalent to `(takeWhile p xs, dropWhile p xs)`.
span ((>) 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])
span ((>) 5) [1,2,3] == ([1,2,3],[])
span ((>) 0) [1,2,3] == ([],[1,2,3])
-}
span : (a -> Bool) -> List a -> ( List a, List a )
span p xs =
    ( takeWhile p xs, dropWhile p xs )


{-| Take elements in order as long as the predicate evaluates to `True`
-}
takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate =
    let
        takeWhileMemo memo list =
            case list of
                [] ->
                    List.reverse memo

                x :: xs ->
                    if predicate x then
                        takeWhileMemo (x :: memo) xs
                    else
                        List.reverse memo
    in
    takeWhileMemo []


{-| Drop elements in order as long as the predicate evaluates to `True`
-}
dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if predicate x then
                dropWhile predicate xs
            else
                list
