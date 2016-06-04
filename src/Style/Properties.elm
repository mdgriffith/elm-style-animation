module Style.Properties exposing (Property (..), Length (..), Angle (..), DisplayMode (..), PathCommand (..), alignStartingPoint) --where

{-|
All animatable properties.

@docs Property

@docs Length, Angle, DisplayMode

@docs PathCommand

@docs alignStartingPoint

-}

import String exposing (concat)
import Color as ElmColor


{-| All currently animatable properties.
-}
type Property a color
    = Display DisplayMode
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
    | Color color
    | BackgroundColor color
    | BorderColor color
    | TransformOrigin a a a Length
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

    -- SVG properties
    | X a
    | Y a
    | Cx a
    | Cy a
    | R a
    | Rx a
    | Ry a
    | D (List (PathCommand a))
    | Points (List (a,a))
    | Fill color
    | Stroke color

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



{-| Describe a path.  To be used in conjunction with the D property for styling svg.

`To` versions of the commands are absolute, while others are relative.

-}
type PathCommand a
    = Move a a
    | MoveTo a a
    | Line a a
    | LineTo a a
    | Horizontal a
    | HorizontalTo a
    | Vertical a
    | VerticalTo a
    | Curve (List (a,a))
    | CurveTo (List (a,a))
    | Quadratic (List (a,a))
    | QuadraticTo (List (a,a))
    | SmoothQuadratic (List (a,a))
    | SmoothQuadraticTo (List (a,a))
    | Smooth (List (a,a))
    | SmoothTo (List (a,a))
    | Arc a a a a
    | ArcTo a a a a
    | LargeArc a a a a
    | LargeArcTo a a a a
    | Close


{-| Given two lists of coordinates, rotate the list so that the lowest coordinate is first.

This is useful to align polygon coordinates so that they can morph smoothely into each other.
-}
alignStartingPoint : List (Float, Float) -> List (Float, Float)
alignStartingPoint points =
    let
        sums = List.map (\(x,y) -> x + y) points
        maybeMin = List.minimum sums

        indexOfLowestPoint =
            case maybeMin of
                Nothing ->
                    Nothing
                Just min ->
                    List.head
                        <| List.filterMap identity
                        <| List.indexedMap (\i val -> if val == min then Just i else Nothing) sums


    in
        case indexOfLowestPoint of
            Nothing -> points
            Just i ->
                (List.drop i points) ++ (List.take i points)
