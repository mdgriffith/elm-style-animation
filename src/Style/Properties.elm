module Style.Properties exposing (StyleProperty(..), id, map, map2, map3, propIs, baseName, render, renderAttr)
-- where
{-|

# All Animatable Style Properties
@docs StyleProperty, DisplayMode

# Units
@docs Length, Angle

-}

import Style.Html.Properties as HtmlProps
import Style.Svg.Properties as SvgProps


type StyleProperty a
     = Html (HtmlProps.Property a)
     | Svg (SvgProps.Property a)




id : StyleProperty a -> String
id prop = 
    case prop of 
        Html property -> "html-" ++ HtmlProps.id property
        Svg property -> "svg-" ++ SvgProps.id property


map : (a -> b) -> StyleProperty a -> StyleProperty b
map fn prop = 
    case prop of 
        Html property -> Html <| HtmlProps.map fn property
        Svg property -> Svg <| SvgProps.map fn property


map2 : (a -> b -> b) -> StyleProperty a -> StyleProperty b -> StyleProperty b
map2 fn prev prop = 
    case prev of
        Html previous -> 
            case prop of 
                Html property -> Html <| HtmlProps.map2 fn previous property
                _ -> prop
        Svg previous -> 
            case prop of 
                Svg property -> Svg <| SvgProps.map2 fn previous property
                _ -> prop


map3 : (a -> b -> c -> c) -> StyleProperty a -> StyleProperty b -> StyleProperty c -> StyleProperty c
map3 fn target prev prop = 
    case target of 
        Html t ->
            case prev of
                Html previous -> 
                    case prop of 
                        Html property -> Html <| HtmlProps.map3 fn t previous property
                        _ -> prop
                _ -> prop
        Svg t ->
            case prev of
                Svg previous -> 
                    case prop of 
                        Svg property -> Svg <| SvgProps.map3 fn t previous property
                        _ -> prop
                _ -> prop





propIs : (a -> Bool) -> StyleProperty a -> Bool
propIs pred prop =
    case prop of 
        Html property -> HtmlProps.propIs pred property
        Svg property -> SvgProps.propIs pred property



--stepTowards : StyleProperty Float -> (Float -> a -> a -> a) -> StyleProperty a -> StyleProperty a -> StyleProperty a
--stepTowards target stepper origin current =






--stepProp : StyleProperty a -> StyleProperty b -> (StyleProperty a -> StyleProperty b -> StyleProperty b) -> StyleProperty b
--stepProp prev prop fn =
--    case prev of 
--        Html prevProp ->
--            case prop of 
--                Html property -> Html <| HtmlProps.stepProp prevProp property fn
--                _ -> prop
--        Svg prevProp ->
--            case prop of 
--                Svg property -> Svg <| SvgProps.stepProp prevProp property fn
--                _ -> prop

baseName : StyleProperty a -> String
baseName prop = 
    case prop of 
        Html property -> HtmlProps.baseName property
        Svg property -> SvgProps.baseName property


render : List (StyleProperty Float) -> List ( String, String )
render props = 
    let 
        renderCSSProp prop = 
            case prop of 
                Html property -> HtmlProps.render [property]
                Svg property -> []
    in
        List.concatMap renderCSSProp props


renderAttr props = 
    let 
        renderAttrProp prop = 
            case prop of 
                Html property -> []
                Svg property -> SvgProps.render [property]
    in
        List.concatMap renderAttrProp props