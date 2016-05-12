module Style.Properties exposing (StyleProperty(..), id, map, propIs, stepProp, baseName, render, renderAttr)
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


propIs : (a -> Bool) -> StyleProperty a -> Bool
propIs pred prop =
    case prop of 
        Html property -> HtmlProps.propIs pred property
        Svg property -> SvgProps.propIs pred property


stepProp : StyleProperty a -> StyleProperty b -> (Maybe b -> a -> a) -> StyleProperty a
stepProp prop prev val =
    case prev of 
        Html prevProp ->
            case prop of 
                Html property -> Html <| HtmlProps.stepProp property prevProp val
                _ -> prop
        Svg prevProp ->
            case prop of 
                Svg property -> Svg <| SvgProps.stepProp property prevProp val
                _ -> prop

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