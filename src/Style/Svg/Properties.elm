module Style.Svg.Properties exposing (Property (..), render, id, map, propIs, stepProp, baseName)
-- where

import Svg.Attributes as Svg

type Property a 
    = X a
    | Y a
    | Cx a
    | Cy a
    | R a
    | Rx a
    | Ry a
    | D a
    | Points a
    --| Transform a
    | Width a
    | Height a
    --| TransformOrigin a a a Length
    --| Matrix a a a a a a
    --| Matrix3d a a a a a a a a a a a a a a a a
    --| Translate a a Length
    --| Translate3d a a a Length
    --| TranslateX a Length
    --| TranslateY a Length
    --| Scale a
    --| Scale3d a a a
    --| ScaleX a
    --| ScaleY a
    --| ScaleZ a
    --| Rotate a Angle
    --| Rotate3d a a a a Angle
    --| RotateX a Angle
    --| RotateY a Angle
    --| Skew a a Angle
    --| SkewX a Angle
    --| SkewY a Angle
    --| Perspective a
    

--render : List (Property Float) -> List ()
render styles =
    let
        toAttr prop =
            case prop of 
                X a -> Svg.x (toString a)
                Y a -> Svg.y (toString a)
                Cx a -> Svg.cx (toString a)
                Cy a -> Svg.cy (toString a)
                R a -> Svg.r (toString a)
                Rx a -> Svg.rx (toString a)
                Ry a -> Svg.ry (toString a)
                D a -> Svg.d (toString a)
                Points a -> Svg.points (toString a)
                Width a -> Svg.width (toString a)
                Height a -> Svg.height (toString a)
    in
        List.map toAttr styles






id : Property a -> String
id prop =
    case prop of 
        X _ -> "x"
        Y _ -> "y"
        Cx _ -> "cx"
        Cy _ -> "cy"
        R _ -> "r"
        Rx _ -> "rx"
        Ry _ -> "ry"
        D _ -> "d"
        Points _ -> "points" 
        --Transform _ -> "transform"
        Width _ -> "width"
        Height _ -> "height"

baseName = id

map : (a -> b) -> Property a -> Property b
map fn prop = 
     case prop of 
        X a -> X (fn a)
        Y a -> Y (fn a)
        Cx a -> Cx (fn a)
        Cy a -> Cy (fn a)
        R a -> R (fn a)
        Rx a -> Rx (fn a)
        Ry a -> Ry (fn a)
        D a -> D (fn a)
        Points a -> Points (fn a)
        --Transform a -> Transform (fn a)
        Width a -> Width (fn a)
        Height a -> Height (fn a)


propIs : (a -> Bool) -> Property a -> Bool
propIs pred prop = 
        case prop of 
            X a -> pred a
            Y a -> pred a
            Cx a -> pred a
            Cy a -> pred a
            R a -> pred a
            Rx a -> pred a
            Ry a -> pred a
            D a -> pred a
            Points a -> pred a
            --Transform a -> Transform pred a
            Width a -> pred a
            Height a -> pred a


stepProp : Property a -> Property b -> (Maybe b -> a -> a) -> Property a
stepProp prop prev val = 
    case prop of
        X to ->
            let
                from =
                    case prev of
                        X x ->
                            Just x

                        _ ->
                            Nothing
            in
                X (val from to)

        Y to ->
            let
                from =
                    case prev of
                        Y x ->
                            Just x

                        _ ->
                            Nothing
            in
                Y (val from to)

        Cx to ->
            let
                from =
                    case prev of
                        Cx x ->
                            Just x

                        _ ->
                            Nothing
            in
                Cx (val from to)

        Cy to ->
            let
                from =
                    case prev of
                        Cy x ->
                            Just x

                        _ ->
                            Nothing
            in
                Cy (val from to)

        R to ->
            let
                from =
                    case prev of
                        R x ->
                            Just x

                        _ ->
                            Nothing
            in
                R (val from to)

        Rx to ->
            let
                from =
                    case prev of
                        Rx x ->
                            Just x

                        _ ->
                            Nothing
            in
                Rx (val from to)

        Ry to ->
            let
                from =
                    case prev of
                        Ry x ->
                            Just x

                        _ ->
                            Nothing
            in
                Ry (val from to)

        D to ->
            let
                from =
                    case prev of
                        D x ->
                            Just x

                        _ ->
                            Nothing
            in
                D (val from to)

        Points to ->
            let
                from =
                    case prev of
                        Points x ->
                            Just x

                        _ ->
                            Nothing
            in
                Points (val from to)

        Width to ->
            let
                from =
                    case prev of
                        Width x ->
                            Just x

                        _ ->
                            Nothing
            in
                Width (val from to)

        Height to ->
            let
                from =
                    case prev of
                        Height x ->
                            Just x

                        _ ->
                            Nothing
            in
                Height (val from to)
