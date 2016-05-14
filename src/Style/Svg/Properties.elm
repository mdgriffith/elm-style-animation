module Style.Svg.Properties exposing (Property (..), render, id, map, map2, map3, propIs, baseName)
-- where

import Svg.Attributes as Svg
import String

type Property a 
    = X a
    | Y a
    | Cx a
    | Cy a
    | R a
    | Rx a
    | Ry a
    | D a
    | Points (List a)
    | Width a
    | Height a
    --| Transform a
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
                Points a -> Svg.points (String.concat <| List.intersperse ", " <| List.map toString a)
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
        Points a -> Points (List.map fn a)
        Width a -> Width (fn a)
        Height a -> Height (fn a)

map2 : (a -> b -> b) -> Property a -> Property b -> Property b
map2 fn prev prop = 
     case prev of 
        X a -> 
            case prop of 
                X b -> X (fn a b)
                _ -> prop
        Y a -> 
            case prop of 
                Y b -> Y (fn a b)
                _ -> prop
        Cx a -> 
            case prop of 
                Cx b -> Cx (fn a b)
                _ -> prop
        Cy a -> 
            case prop of 
                Cy b -> Cy (fn a b)
                _ -> prop
        R a -> 
            case prop of 
                R b -> R (fn a b)
                _ -> prop
        Rx a -> 
            case prop of 
                Rx b -> Rx (fn a b)
                _ -> prop
        Ry a -> 
            case prop of 
                Ry b -> Ry (fn a b)
                _ -> prop
        D a -> 
            case prop of 
                D b -> D (fn a b)
                _ -> prop
        Points a -> 
            case prop of 
                Points b -> Points <| List.map2 fn a b
                _ -> prop

        Width a -> 
            case prop of 
                Width b -> Width (fn a b)
                _ -> prop
        Height a -> 
            case prop of 
                Height b -> Height (fn a b)
                _ -> prop


map3 : (a -> b -> c -> c) -> Property a -> Property b -> Property c -> Property c
map3 fn target prev prop =
    case target of
        X a1 ->
            case prev of
                X a2 ->
                    case prop of
                        X a3 ->
                            X (fn a1 a2 a3)
                        _ -> prop

                _ -> prop

        Y a1 ->
            case prev of
                Y a2 ->
                    case prop of
                        Y a3 ->
                            Y (fn a1 a2 a3)
                        _ -> prop

                _ -> prop

        Cx a1 ->
            case prev of
                Cx a2 ->
                    case prop of
                        Cx a3 ->
                            Cx (fn a1 a2 a3)
                        _ -> prop

                _ -> prop

        Cy a1 ->
            case prev of
                Cy a2 ->
                    case prop of
                        Cy a3 ->
                            Cy (fn a1 a2 a3)
                        _ -> prop

                _ -> prop

        R a1 ->
            case prev of
                R a2 ->
                    case prop of
                        R a3 ->
                            R (fn a1 a2 a3)
                        _ -> prop

                _ -> prop

        Rx a1 ->
            case prev of
                Rx a2 ->
                    case prop of
                        Rx a3 ->
                            Rx (fn a1 a2 a3)
                        _ -> prop

                _ -> prop

        Ry a1 ->
            case prev of
                Ry a2 ->
                    case prop of
                        Ry a3 ->
                            Ry (fn a1 a2 a3)
                        _ -> prop

                _ -> prop

        D a1 ->
            case prev of
                D a2 ->
                    case prop of
                        D a3 ->
                            D (fn a1 a2 a3)
                        _ -> prop

                _ -> prop

        Points props1 ->
            case prev of
                Points props2 ->
                    case prop of
                        Points props3 ->
                            Points <| List.map3 fn props1 props2 props3
                        _ -> prop

                _ -> prop

        Width a1 ->
            case prev of
                Width a2 ->
                    case prop of
                        Width a3 ->
                            Width (fn a1 a2 a3)
                        _ -> prop

                _ -> prop

        Height a1 ->
            case prev of
                Height a2 ->
                    case prop of
                        Height a3 ->
                            Height (fn a1 a2 a3)
                        _ -> prop

                _ -> prop



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
            Points a -> List.all pred a
            --Transform a -> Transform pred a
            Width a -> pred a
            Height a -> pred a


--stepProp : Property a -> Property b -> (Property a -> Property b -> Property b) -> Property b
--stepProp prev prop fn = 
--        fn prev prop
    --case prop of
    --    X to ->
    --        let
    --            from =
    --                case prev of
    --                    X x ->
    --                        Just x

    --                    _ ->
    --                        Nothing
    --        in
    --            X (fn from to)

    --    Y to ->
    --        let
    --            from =
    --                case prev of
    --                    Y x ->
    --                        Just x

    --                    _ ->
    --                        Nothing
    --        in
    --            Y (fn from to)

    --    Cx to ->
    --        let
    --            from =
    --                case prev of
    --                    Cx x ->
    --                        Just x

    --                    _ ->
    --                        Nothing
    --        in
    --            Cx (fn from to)

    --    Cy to ->
    --        let
    --            from =
    --                case prev of
    --                    Cy x ->
    --                        Just x

    --                    _ ->
    --                        Nothing
    --        in
    --            Cy (fn from to)

    --    R to ->
    --        let
    --            from =
    --                case prev of
    --                    R x ->
    --                        Just x

    --                    _ ->
    --                        Nothing
    --        in
    --            R (fn from to)

    --    Rx to ->
    --        let
    --            from =
    --                case prev of
    --                    Rx x ->
    --                        Just x

    --                    _ ->
    --                        Nothing
    --        in
    --            Rx (fn from to)

    --    Ry to ->
    --        let
    --            from =
    --                case prev of
    --                    Ry x ->
    --                        Just x

    --                    _ ->
    --                        Nothing
    --        in
    --            Ry (fn from to)

    --    D to ->
    --        let
    --            from =
    --                case prev of
    --                    D x ->
    --                        Just x

    --                    _ ->
    --                        Nothing
    --        in
    --            D (fn from to)

    --    Points to ->
    --        let
    --            from =
    --                case prev of
    --                    Points x ->
    --                        List.map Just x

    --                    _ ->
    --                        List.repeat (List.length to) Nothing
    --        in
    --            Points (List.map2 fn from to)

    --    Width to ->
    --        let
    --            from =
    --                case prev of
    --                    Width x ->
    --                        Just x

    --                    _ ->
    --                        Nothing
    --        in
    --            Width (fn from to)

    --    Height to ->
    --        let
    --            from =
    --                case prev of
    --                    Height x ->
    --                        Just x

    --                    _ ->
    --                        Nothing
    --        in
    --            Height (fn from to)
