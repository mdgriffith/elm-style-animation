module Html.Animation.Render (name, value, id) where


import Html.Animation.Properties exposing (..)

import String exposing (concat)


name : StyleProperty a -> String
name styleProp =
  case styleProp of
    Prop str _ _ ->
      str

    Opacity _ ->
      "opacity"

    Height _ _ ->
      "height"

    Width _ _ ->
      "width"

    Left _ _ ->
      "left"

    Right _ _ ->
      "right"

    Bottom _ _ ->
      "bottom"

    Top _ _ ->
      "top"

    MaxHeight _ _ ->
      "max-height"

    MaxWidth _ _ ->
      "max-width"

    MinHeight _ _ ->
      "min-height"

    MinWidth _ _ ->
      "min-width"

    Padding _ _ ->
      "padding"

    PaddingLeft _ _ ->
      "padding-left"

    PaddingRight _ _ ->
      "padding-right"

    PaddingTop _ _ ->
      "padding-top"

    PaddingBottom _ _ ->
      "padding-bottom"

    Margin _ _ ->
      "margin"

    MarginLeft _ _ ->
      "margin-left"

    MarginRight _ _ ->
      "margin-right"

    MarginTop _ _ ->
      "margin-top"

    MarginBottom _ _ ->
      "margin-bottom"

    BorderWidth _ _ ->
      "border-width"

    BorderRadius _ _ ->
      "border-radius"

    BorderTopLeftRadius _ _ ->
      "border-top-left-radius"

    BorderTopRightRadius _ _ ->
      "border-top-right-radius"

    BorderBottomLeftRadius _ _ ->
      "border-bottom-left-radius"

    BorderBottomRightRadius _ _ ->
      "border-bottom-right-radius"

    LetterSpacing _ _ ->
      "letter-spacing"

    LineHeight _ _ ->
      "line-height"

    BackgroundPosition _ _ _ ->
      "background-position"

    TransformOrigin _ _ _ _ ->
      "transform-origin"

    Color _ _ _ _ ->
      "color"

    BackgroundColor _ _ _ _ ->
      "background-color"

    BorderColor _ _ _ _ ->
      "border-color"

    Matrix _ _ _ _ _ _ ->
      "transform"

    Matrix3d _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ->
      "transform"

    Translate _ _ _ ->
      "transform"

    Translate3d _ _ _ _ ->
      "transform"

    TranslateX _ _ ->
      "transform"

    TranslateY _ _ ->
      "transform"

    Scale _ ->
      "transform"

    Scale3d _ _ _ ->
      "transform"

    ScaleX _ ->
      "transform"

    ScaleY _ ->
      "transform"

    ScaleZ _ ->
      "transform"

    Rotate _ _ ->
      "transform"

    Rotate3d _ _ _ _ _ ->
      "transform"

    RotateX _ _ ->
      "transform"

    RotateY _ _ ->
      "transform"

    Skew _ _ _ ->
      "transform"

    SkewX _ _ ->
      "transform"

    SkewY _ _ ->
      "transform"

    Perspective _ ->
      "transform"



-- renders a valid css value for a Style Property
value : StyleProperty Static -> String
value prop =
  let
    val a =
      toString a

    renderLength a unit =
      (val a) ++ lenUnit unit

    renderAngle a unit =
      (val a) ++ angleUnit unit

    renderList xs =
      "("
        ++ (String.concat
              <| List.intersperse ","
              <| List.map toString xs
           )
        ++ ")"
  in
    case prop of
      Prop _ a u ->
        (val a) ++ u

      Opacity a ->
        val a

      Height a unit ->
        renderLength a unit

      Width a unit ->
        renderLength a unit

      Left a unit ->
        renderLength a unit

      Top a unit ->
        renderLength a unit

      Right a unit ->
        renderLength a unit

      Bottom a unit ->
        renderLength a unit

      MaxHeight a unit ->
        renderLength a unit

      MaxWidth a unit ->
        renderLength a unit

      MinHeight a unit ->
        renderLength a unit

      MinWidth a unit ->
        renderLength a unit

      Padding a unit ->
        renderLength a unit

      PaddingLeft a unit ->
        renderLength a unit

      PaddingRight a unit ->
        renderLength a unit

      PaddingTop a unit ->
        renderLength a unit

      PaddingBottom a unit ->
        renderLength a unit

      Margin a unit ->
        renderLength a unit

      MarginLeft a unit ->
        renderLength a unit

      MarginRight a unit ->
        renderLength a unit

      MarginTop a unit ->
        renderLength a unit

      MarginBottom a unit ->
        renderLength a unit

      BorderWidth a unit ->
        renderLength a unit

      BorderRadius a unit ->
        renderLength a unit

      BorderTopLeftRadius a unit ->
        renderLength a unit

      BorderTopRightRadius a unit ->
        renderLength a unit

      BorderBottomLeftRadius a unit ->
        renderLength a unit

      BorderBottomRightRadius a unit ->
        renderLength a unit

      LetterSpacing a unit ->
        renderLength a unit

      LineHeight a unit ->
        renderLength a unit

      BackgroundPosition x y unit ->
        renderLength x unit
          ++ " "
          ++ renderLength y unit

      TransformOrigin x y z unit ->
        renderLength x unit
          ++ " "
          ++ renderLength y unit
          ++ " "
          ++ renderLength z unit

      Color x y z a ->
        renderColor x y z a

      BackgroundColor x y z a ->
        renderColor x y z a

      BorderColor x y z a ->
        renderColor x y z a

      Translate a1 a2 unit ->
        "translate("
          ++ (renderLength a1 unit)
          ++ ","
          ++ (renderLength a2 unit)
          ++ ")"

      Translate3d a1 a2 a3 unit ->
        "translate3d("
          ++ (renderLength a1 unit)
          ++ ","
          ++ (renderLength a2 unit)
          ++ ","
          ++ (renderLength a3 unit)
          ++ ")"

      TranslateX a unit ->
        "translateX(" ++ renderLength a unit ++ ")"

      TranslateY a unit ->
        "translateY(" ++ renderLength a unit ++ ")"

      Scale a1 ->
        "scale(" ++ (val a1) ++ ")"

      Scale3d a1 a2 a3 ->
        "scale3d("
          ++ (val a1)
          ++ ","
          ++ (val a2)
          ++ ","
          ++ (val a3)
          ++ ")"

      ScaleX a ->
        "scaleX(" ++ val a ++ ")"

      ScaleY a ->
        "scaleY(" ++ val a ++ ")"

      ScaleZ a ->
        "scaleZ(" ++ val a ++ ")"

      Rotate a unit ->
        "rotate(" ++ renderAngle a unit ++ ")"

      Rotate3d a1 a2 a3 a4 unit ->
        "rotate3d("
          ++ (val a1)
          ++ ","
          ++ (val a2)
          ++ ","
          ++ (val a3)
          ++ ","
          ++ (renderAngle a4 unit)
          ++ ")"

      RotateX a unit ->
        "rotateX(" ++ renderAngle a unit ++ ")"

      RotateY a unit ->
        "rotateY(" ++ renderAngle a unit ++ ")"

      Skew a1 a2 unit ->
        "skew("
          ++ (renderAngle a1 unit)
          ++ ","
          ++ (renderAngle a2 unit)
          ++ ")"

      SkewX a unit ->
        "skewX(" ++ renderAngle a unit ++ ")"

      SkewY a unit ->
        "skewY(" ++ renderAngle a unit ++ ")"

      Perspective a ->
        "perspective(" ++ (val a) ++ ")"

      Matrix a b c x y z ->
        "matrix"
          ++ (renderList [ a, b, c, x, y, z ])

      Matrix3d a b c d e f g h i j k l m n o p ->
        "matrix3d"
          ++ (renderList [ a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p ])


renderColor : Float -> Float -> Float -> Float -> String
renderColor x y z a =
  let
    renderList xs =
      "("
        ++ (String.concat
              <| List.intersperse ","
              <| List.map toString xs
           )
        ++ ")"

    renderIntList xs =
      renderList <| List.map round xs
  in
    "rgba("
      ++ toString (round x)
      ++ ","
      ++ toString (round y)
      ++ ","
      ++ toString (round z)
      ++ ","
      ++ toString a
      ++ ")"




id : StyleProperty a -> String
id prop =
  case prop of
    Prop name _ unit ->
      name ++ unit

    Opacity _ ->
      "opacity"

    Height _ unit ->
      "height" ++ lenUnit unit

    Width _ unit ->
      "width" ++ lenUnit unit

    Left _ unit ->
      "left" ++ lenUnit unit

    Right _ unit ->
      "right" ++ lenUnit unit

    Bottom _ unit ->
      "bottom" ++ lenUnit unit

    Top _ unit ->
      "top" ++ lenUnit unit

    MaxHeight _ unit ->
      "max-height" ++ lenUnit unit

    MaxWidth _ unit ->
      "max-width" ++ lenUnit unit

    MinHeight _ unit ->
      "min-height" ++ lenUnit unit

    MinWidth _ unit ->
      "min-width" ++ lenUnit unit

    Padding _ unit ->
      "padding" ++ lenUnit unit

    PaddingLeft _ unit ->
      "padding-left" ++ lenUnit unit

    PaddingRight _ unit ->
      "padding-right" ++ lenUnit unit

    PaddingTop _ unit ->
      "padding-top" ++ lenUnit unit

    PaddingBottom _ unit ->
      "padding-bottom" ++ lenUnit unit

    Margin _ unit ->
      "margin" ++ lenUnit unit

    MarginLeft _ unit ->
      "margin-left" ++ lenUnit unit

    MarginRight _ unit ->
      "margin-right" ++ lenUnit unit

    MarginTop _ unit ->
      "margin-top" ++ lenUnit unit

    MarginBottom _ unit ->
      "margin-bottom" ++ lenUnit unit

    BorderWidth _ unit ->
      "border-width" ++ lenUnit unit

    BorderRadius _ unit ->
      "border-radius" ++ lenUnit unit

    BorderTopLeftRadius _ unit ->
      "border-top-left-radius" ++ lenUnit unit

    BorderTopRightRadius _ unit ->
      "border-top-right-radius" ++ lenUnit unit

    BorderBottomLeftRadius _ unit ->
      "border-bottom-left-radius" ++ lenUnit unit

    BorderBottomRightRadius _ unit ->
      "border-bottom-right-radius" ++ lenUnit unit

    LetterSpacing _ unit ->
      "letter-spacing" ++ lenUnit unit

    LineHeight _ unit ->
      "line-height" ++ lenUnit unit

    BackgroundPosition _ _ unit ->
      "background-position" ++ lenUnit unit

    Color _ _ _ _ ->
      "color"

    BackgroundColor _ _ _ _ ->
      "background-color"

    BorderColor _ _ _ _ ->
      "border-color"

    TransformOrigin _ _ _ unit ->
      "transform-origin" ++ lenUnit unit

    Matrix _ _ _ _ _ _ ->
      "matrix"

    Matrix3d _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ->
      "matrix3d"

    Translate _ _ unit ->
      "translate" ++ lenUnit unit

    Translate3d _ _ _ unit ->
      "translate3d" ++ lenUnit unit

    TranslateX _ unit ->
      "translatex" ++ lenUnit unit

    TranslateY _ unit ->
      "translatey" ++ lenUnit unit

    Scale _ ->
      "scale"

    Scale3d _ _ _ ->
      "scale3d"

    ScaleX _ ->
      "scalex"

    ScaleY _ ->
      "scaley"

    ScaleZ _ ->
      "scalez"

    Rotate _ unit ->
      "rotate" ++ angleUnit unit

    Rotate3d _ _ _ _ unit ->
      "rotate3d" ++ angleUnit unit

    RotateX _ unit ->
      "rotatex" ++ angleUnit unit

    RotateY _ unit ->
      "rotatey" ++ angleUnit unit

    Skew _ _ unit ->
      "skew" ++ angleUnit unit

    SkewX _ unit ->
      "skewx" ++ angleUnit unit

    SkewY _ unit ->
      "skewy" ++ angleUnit unit

    Perspective _ ->
      "perspective"


lenUnit : Length -> String
lenUnit unit =
  case unit of
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


angleUnit : Angle -> String
angleUnit unit =
  case unit of
    Deg ->
      "deg"

    Grad ->
      "grad"

    Rad ->
      "rad"

    Turn ->
      "turn"

