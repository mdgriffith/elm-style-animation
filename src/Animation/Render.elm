module Animation.Render exposing (..)

{-| The actual internals of rendering

-}

import Html
import Html.Attributes
import Svg.Attributes
import Animation.Model exposing (..)


renderValues : Animation msgA -> ( List ( String, String ), List Property )
renderValues (Animation model) =
    let
        ( attrProps, styleProps ) =
            List.partition isAttr model.style

        ( style, transforms, filters ) =
            List.foldl
                (\prop ( style, transforms, filters ) ->
                    if isTransformation prop then
                        ( style
                        , transforms ++ [ prop ]
                        , filters
                        )
                    else if isFilter prop then
                        ( style
                        , transforms
                        , filters ++ [ prop ]
                        )
                    else
                        ( style ++ [ prop ]
                        , transforms
                        , filters
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
                                    propertyName prop ++ "(" ++ (propertyValue prop ", ") ++ ")"
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
                                        "url(\"" ++ (propertyValue prop ", ") ++ "\")"
                                    else
                                        propertyName prop ++ "(" ++ (propertyValue prop ", ") ++ ")"
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
            Html.Attributes.style <|
                List.concatMap prefix style

        otherAttrs =
            List.filterMap renderAttrs attrProps
    in
        styleAttr :: otherAttrs


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
                ++ toString x.position
                ++ x.unit
                ++ ") rotateY("
                ++ toString y.position
                ++ y.unit
                ++ ") rotateZ("
                ++ toString z.position
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
        || case prop of
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


propertyValue : Animation.Model.Property -> String -> String
propertyValue prop delim =
    case prop of
        ExactProperty _ value ->
            value

        ColorProperty _ r g b a ->
            "rgba("
                ++ toString (round r.position)
                ++ ","
                ++ toString (round g.position)
                ++ ","
                ++ toString (round b.position)
                ++ ","
                ++ toString a.position
                ++ ")"

        ShadowProperty name inset shadow ->
            (if inset then
                "inset "
             else
                ""
            )
                ++ toString shadow.offsetX.position
                ++ "px"
                ++ " "
                ++ toString shadow.offsetY.position
                ++ "px"
                ++ " "
                ++ toString shadow.blur.position
                ++ "px"
                ++ " "
                ++ (if name == "text-shadow" || name == "drop-shadow" then
                        ""
                    else
                        toString shadow.size.position
                            ++ "px"
                            ++ " "
                   )
                ++ "rgba("
                ++ toString (round shadow.red.position)
                ++ ", "
                ++ toString (round shadow.green.position)
                ++ ", "
                ++ toString (round shadow.blue.position)
                ++ ", "
                ++ toString shadow.alpha.position
                ++ ")"

        Property _ x ->
            toString x.position ++ x.unit

        Property2 _ x y ->
            toString x.position
                ++ x.unit
                ++ delim
                ++ toString y.position
                ++ y.unit

        Property3 _ x y z ->
            toString x.position
                ++ x.unit
                ++ delim
                ++ toString y.position
                ++ y.unit
                ++ delim
                ++ toString z.position
                ++ z.unit

        Property4 _ w x y z ->
            toString w.position
                ++ w.unit
                ++ delim
                ++ toString x.position
                ++ x.unit
                ++ delim
                ++ toString y.position
                ++ y.unit
                ++ delim
                ++ toString z.position
                ++ z.unit

        AngleProperty _ x ->
            toString x.position ++ x.unit

        Points coords ->
            String.join " " <|
                List.map
                    (\( x, y ) ->
                        toString x.position ++ "," ++ toString y.position
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
                        toString x.position ++ "," ++ toString y.position
                    )
                    coords
    in
        case cmd of
            Move x y ->
                "m " ++ toString x.position ++ "," ++ toString y.position

            MoveTo x y ->
                "M " ++ toString x.position ++ "," ++ toString y.position

            Line x y ->
                "l " ++ toString x.position ++ "," ++ toString y.position

            LineTo x y ->
                "L " ++ toString x.position ++ "," ++ toString y.position

            Horizontal a ->
                "h " ++ toString a.position

            HorizontalTo a ->
                "H " ++ toString a.position

            Vertical a ->
                "v " ++ toString a.position

            VerticalTo a ->
                "V " ++ toString a.position

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
                        ++ (toString <| c1x.position)
                        ++ " "
                        ++ (toString <| c1y.position)
                        ++ ", "
                        ++ (toString <| c2x.position)
                        ++ " "
                        ++ (toString <| c2y.position)
                        ++ ", "
                        ++ (toString <| p1x.position)
                        ++ " "
                        ++ (toString <| p1y.position)

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
                        ++ (toString <| c1x.position)
                        ++ " "
                        ++ (toString <| c1y.position)
                        ++ ", "
                        ++ (toString <| c2x.position)
                        ++ " "
                        ++ (toString <| c2y.position)
                        ++ ", "
                        ++ (toString <| p1x.position)
                        ++ " "
                        ++ (toString <| p1y.position)

            Quadratic { control, point } ->
                let
                    ( c1x, c1y ) =
                        control

                    ( p1x, p1y ) =
                        point
                in
                    "q "
                        ++ (toString <| c1x.position)
                        ++ " "
                        ++ (toString <| c1y.position)
                        ++ ", "
                        ++ (toString <| p1x.position)
                        ++ " "
                        ++ (toString <| p1y.position)

            QuadraticTo { control, point } ->
                let
                    ( c1x, c1y ) =
                        control

                    ( p1x, p1y ) =
                        point
                in
                    "Q "
                        ++ (toString <| c1x.position)
                        ++ " "
                        ++ (toString <| c1y.position)
                        ++ ", "
                        ++ (toString <| p1x.position)
                        ++ " "
                        ++ (toString <| p1y.position)

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
                                ++ toString arc.radius.position
                                ++ ","
                                ++ toString arc.radius.position
                                ++ ",0,1,1,"
                                ++ toString (arc.x.position - dx)
                                ++ ","
                                ++ toString (arc.y.position - dy)
                                ++ " A "
                                ++ toString arc.radius.position
                                ++ ","
                                ++ toString arc.radius.position
                                ++ ",0,1,1,"
                                ++ toString (arc.x.position + dx)
                                ++ ","
                                ++ toString (arc.y.position + dy)
                    else
                        "A "
                            ++ toString arc.radius.position
                            ++ ","
                            ++ toString arc.radius.position
                            ++ " 0 "
                            ++ (if deltaAngle >= 180 then
                                    "1"
                                else
                                    "0"
                               )
                            ++ " "
                            ++ "1"
                            ++ " "
                            ++ toString (arc.x.position + (arc.radius.position * (cos <| degrees arc.endAngle.position)))
                            ++ ","
                            ++ toString (arc.y.position + (arc.radius.position * (sin <| degrees arc.endAngle.position)))

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
                                ++ toString arc.radius.position
                                ++ ","
                                ++ toString arc.radius.position
                                ++ ",0,1,0,"
                                ++ toString (arc.x.position - dx)
                                ++ ","
                                ++ toString (arc.y.position - dy)
                                ++ " A "
                                ++ toString arc.radius.position
                                ++ ","
                                ++ toString arc.radius.position
                                ++ ",0,1,1,"
                                ++ toString (arc.x.position + dx)
                                ++ ","
                                ++ toString (arc.y.position + dy)
                    else
                        "A "
                            ++ toString arc.radius.position
                            ++ ","
                            ++ toString arc.radius.position
                            ++ " 0 "
                            ++ (if arc.startAngle.position - arc.endAngle.position >= 180 then
                                    "1"
                                else
                                    "0"
                               )
                            ++ " "
                            ++ "0"
                            ++ " "
                            ++ toString (arc.x.position + (arc.radius.position * (cos arc.endAngle.position)))
                            ++ ","
                            ++ toString (arc.y.position + (arc.radius.position * (sin arc.endAngle.position)))

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
                                    Debug.log "elm-style-animation" ("The \"" ++ name ++ "\" css property is listed more than once.  Only the last instance will be used.")
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
                    if (predicate x) then
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
            if (predicate x) then
                dropWhile predicate xs
            else
                list
