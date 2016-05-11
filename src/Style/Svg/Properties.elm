module Style.Svg.Properties exposing (Property (..), render, id, mapProp, propIs, stepProp, baseName)
-- where


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
    | Transform a
    | Width a
    | Height a
    

render : List (Property Float) -> List ( String, String )
render styles = []


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
        Transform _ -> "transform"
        Width _ -> "width"
        Height _ -> "height"

baseName = id

mapProp : (a -> b) -> Property a -> Property b
mapProp fn prop = 
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
        Transform a -> Transform (fn a)
        Width a -> Width (fn a)
        Height a -> Height (fn a)


propIs : (a -> Bool) -> Property a -> Bool
propIs pred prop = True


stepProp : Property a -> Property b -> (Maybe b -> a -> a) -> Property a
stepProp prop prev val = prop