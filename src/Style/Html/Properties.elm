module Style.Html.Properties exposing (..) --where


import String exposing (concat)


{-| All currently animatable properties.
-}
type Property a
    = Prop String a String
    | Display DisplayMode
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
    | Color a a a a
    | BackgroundColor a a a a
    | BorderColor a a a a
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



{-| Render style properties into their css values.

-}
render : List (Property Float) -> List ( String, String )
render styleProps =
  let
    rendered =
      List.map (\prop -> ( name prop, value prop )) styleProps

    props =
      List.filter (\( name, _ ) -> name /= "transform") rendered

    transforms =
      List.map (snd)
        <| List.filter (\( name, _ ) -> name == "transform") rendered

    combinedTransforms =
      if List.length transforms == 0 then
        []
      else
        [ ( "transform"
          , String.concat
              (List.intersperse
                " "
                transforms
              )
          )
        ]
  in
    List.concatMap prefix (props ++ combinedTransforms)


name : Property a -> String
name styleProp =
  case styleProp of
    Prop str _ _ ->
      str

    Display _ ->
      "display"

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


value : Property Float -> String
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

      Display mode ->
        displayMode mode

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
      fst stylePair

    propValue =
      snd stylePair
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

      _ ->
        [ stylePair ]


{-| Used to match properties without units so that mismatched units can be detected.

-}
baseName : Property a -> String
baseName prop =
  case prop of
    Prop name _ unit ->
      name ++ unit

    Display _ ->
      "display"

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

    Color _ _ _ _ ->
      "color"

    BackgroundColor _ _ _ _ ->
      "background-color"

    BorderColor _ _ _ _ ->
      "border-color"

    TransformOrigin _ _ _ _ ->
      "transform-origin"

    Matrix _ _ _ _ _ _ ->
      "matrix"

    Matrix3d _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ->
      "matrix3d"

    Translate _ _ _ ->
      "translate"

    Translate3d _ _ _ _ ->
      "translate3d"

    TranslateX _ _ ->
      "translatex"

    TranslateY _ _ ->
      "translatey"

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

    Rotate _ _ ->
      "rotate"

    Rotate3d _ _ _ _ _ ->
      "rotate3d"

    RotateX _ _ ->
      "rotatex"

    RotateY _ _ ->
      "rotatey"

    Skew _ _ _ ->
      "skew"

    SkewX _ _ ->
      "skewx"

    SkewY _ _ ->
      "skewy"

    Perspective _ ->
      "perspective"


id : Property a -> String
id prop =
  case prop of
    Prop name _ unit ->
      name ++ unit

    Opacity _ ->
      "opacity"

    Display _ ->
      "display"

    Height _ unit ->
      "height in " ++ lenUnit unit

    Width _ unit ->
      "width in " ++ lenUnit unit

    Left _ unit ->
      "left in " ++ lenUnit unit

    Right _ unit ->
      "right in " ++ lenUnit unit

    Bottom _ unit ->
      "bottom in " ++ lenUnit unit

    Top _ unit ->
      "top in " ++ lenUnit unit

    MaxHeight _ unit ->
      "max-height in " ++ lenUnit unit

    MaxWidth _ unit ->
      "max-width in " ++ lenUnit unit

    MinHeight _ unit ->
      "min-height in " ++ lenUnit unit

    MinWidth _ unit ->
      "min-width in " ++ lenUnit unit

    Padding _ unit ->
      "padding in " ++ lenUnit unit

    PaddingLeft _ unit ->
      "padding-left in " ++ lenUnit unit

    PaddingRight _ unit ->
      "padding-right in " ++ lenUnit unit

    PaddingTop _ unit ->
      "padding-top in " ++ lenUnit unit

    PaddingBottom _ unit ->
      "padding-bottom in " ++ lenUnit unit

    Margin _ unit ->
      "margin in " ++ lenUnit unit

    MarginLeft _ unit ->
      "margin-left in " ++ lenUnit unit

    MarginRight _ unit ->
      "margin-right in " ++ lenUnit unit

    MarginTop _ unit ->
      "margin-top in " ++ lenUnit unit

    MarginBottom _ unit ->
      "margin-bottom in " ++ lenUnit unit

    BorderWidth _ unit ->
      "border-width in " ++ lenUnit unit

    BorderRadius _ unit ->
      "border-radius in " ++ lenUnit unit

    BorderTopLeftRadius _ unit ->
      "border-top-left-radius in " ++ lenUnit unit

    BorderTopRightRadius _ unit ->
      "border-top-right-radius in " ++ lenUnit unit

    BorderBottomLeftRadius _ unit ->
      "border-bottom-left-radius in " ++ lenUnit unit

    BorderBottomRightRadius _ unit ->
      "border-bottom-right-radius in " ++ lenUnit unit

    LetterSpacing _ unit ->
      "letter-spacing in " ++ lenUnit unit

    LineHeight _ unit ->
      "line-height in " ++ lenUnit unit

    BackgroundPosition _ _ unit ->
      "background-position in " ++ lenUnit unit

    Color _ _ _ _ ->
      "color"

    BackgroundColor _ _ _ _ ->
      "background-color"

    BorderColor _ _ _ _ ->
      "border-color"

    TransformOrigin _ _ _ unit ->
      "transform-origin in " ++ lenUnit unit

    Matrix _ _ _ _ _ _ ->
      "matrix"

    Matrix3d _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ->
      "matrix3d"

    Translate _ _ unit ->
      "translate in " ++ lenUnit unit

    Translate3d _ _ _ unit ->
      "translate3d in " ++ lenUnit unit

    TranslateX _ unit ->
      "translatex in " ++ lenUnit unit

    TranslateY _ unit ->
      "translatey in " ++ lenUnit unit

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
      "rotate in " ++ angleUnit unit

    Rotate3d _ _ _ _ unit ->
      "rotate3d in " ++ angleUnit unit

    RotateX _ unit ->
      "rotatex in " ++ angleUnit unit

    RotateY _ unit ->
      "rotatey in " ++ angleUnit unit

    Skew _ _ unit ->
      "skew in " ++ angleUnit unit

    SkewX _ unit ->
      "skewx in " ++ angleUnit unit

    SkewY _ unit ->
      "skewy in " ++ angleUnit unit

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


displayMode : DisplayMode -> String
displayMode mode =
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




-------------------------------
-- Mapping
-------------------------------


mapProp : (a -> b) -> Property a -> Property b
mapProp fn prop =
    case prop of
        Prop n a u ->
            Prop n (fn a) u

        Display mode ->
            Display mode

        --case mode of
        --  DisplayMode a p t ->
        --        Display (DisplayMode (fn a) p t)
        Opacity a ->
            Opacity (fn a)

        Height a u ->
            Height (fn a) u

        Width a u ->
            Width (fn a) u

        Left a u ->
            Left (fn a) u

        Top a u ->
            Top (fn a) u

        Right a u ->
            Right (fn a) u

        Bottom a u ->
            Bottom (fn a) u

        MaxHeight a u ->
            MaxHeight (fn a) u

        MaxWidth a u ->
            MaxWidth (fn a) u

        MinHeight a u ->
            MinHeight (fn a) u

        MinWidth a u ->
            MinWidth (fn a) u

        Padding a u ->
            Padding (fn a) u

        PaddingLeft a u ->
            PaddingLeft (fn a) u

        PaddingRight a u ->
            PaddingRight (fn a) u

        PaddingTop a u ->
            PaddingTop (fn a) u

        PaddingBottom a u ->
            PaddingBottom (fn a) u

        Margin a u ->
            Margin (fn a) u

        MarginLeft a u ->
            MarginLeft (fn a) u

        MarginRight a u ->
            MarginRight (fn a) u

        MarginTop a u ->
            MarginTop (fn a) u

        MarginBottom a u ->
            MarginBottom (fn a) u

        BorderWidth a u ->
            BorderWidth (fn a) u

        BorderRadius a u ->
            BorderRadius (fn a) u

        BorderTopLeftRadius a u ->
            BorderTopLeftRadius (fn a) u

        BorderTopRightRadius a u ->
            BorderTopRightRadius (fn a) u

        BorderBottomLeftRadius a u ->
            BorderBottomLeftRadius (fn a) u

        BorderBottomRightRadius a u ->
            BorderBottomRightRadius (fn a) u

        LetterSpacing a u ->
            LetterSpacing (fn a) u

        LineHeight a u ->
            LineHeight (fn a) u

        BackgroundPosition x y u ->
            BackgroundPosition (fn x) (fn y) u

        TransformOrigin x y z u ->
            TransformOrigin (fn x) (fn y) (fn z) u

        Color x y z a ->
            Color (fn x) (fn y) (fn z) (fn a)

        BackgroundColor x y z a ->
            BackgroundColor (fn x) (fn y) (fn z) (fn a)

        BorderColor x y z a ->
            BorderColor (fn x) (fn y) (fn z) (fn a)

        Translate a1 a2 u ->
            Translate (fn a1) (fn a2) u

        Translate3d a1 a2 a3 u ->
            Translate3d (fn a1) (fn a2) (fn a3) u

        TranslateX a u ->
            TranslateX (fn a) u

        TranslateY a u ->
            TranslateY (fn a) u

        Scale a ->
            Scale (fn a)

        Scale3d a1 a2 a3 ->
            Scale3d (fn a1) (fn a2) (fn a3)

        ScaleX a ->
            ScaleX (fn a)

        ScaleY a ->
            ScaleY (fn a)

        ScaleZ a ->
            ScaleZ (fn a)

        Rotate a u ->
            Rotate (fn a) u

        Rotate3d a1 a2 a3 a4 u ->
            Rotate3d (fn a1) (fn a2) (fn a3) (fn a4) u

        RotateX a u ->
            RotateX (fn a) u

        RotateY a u ->
            RotateY (fn a) u

        Skew a1 a2 u ->
            Skew (fn a1) (fn a2) u

        SkewX a u ->
            SkewX (fn a) u

        SkewY a u ->
            SkewY (fn a) u

        Perspective a ->
            Perspective (fn a)

        Matrix a b c x y z ->
            Matrix (fn a) (fn b) (fn c) (fn x) (fn y) (fn z)

        Matrix3d a b c d e f g h i j k l m n o p ->
            Matrix3d (fn a) (fn b) (fn c) (fn d) (fn e) (fn f) (fn g) (fn h) (fn i) (fn j) (fn k) (fn l) (fn m) (fn n) (fn o) (fn p)



propIs : (a -> Bool) -> Property a -> Bool
propIs pred prop =
    --let
    --    pred prop =
    --        case prop.easing of
    --            Nothing ->
    --                Spring.atRest prop.spring prop.physical

    --            Just easing ->
    --                time
    --                    >= easing.duration
    --                    && easing.counterForcePhys
    --                    == Nothing
    --in
        case prop of
            Prop _ a _ ->
                pred a

            Display mode ->
                True

            Opacity a ->
                pred a

            Height a _ ->
                pred a

            Width a _ ->
                pred a

            Left a _ ->
                pred a

            Top a _ ->
                pred a

            Right a _ ->
                pred a

            Bottom a _ ->
                pred a

            MaxHeight a _ ->
                pred a

            MaxWidth a _ ->
                pred a

            MinHeight a _ ->
                pred a

            MinWidth a _ ->
                pred a

            Padding a _ ->
                pred a

            PaddingLeft a _ ->
                pred a

            PaddingRight a _ ->
                pred a

            PaddingTop a _ ->
                pred a

            PaddingBottom a _ ->
                pred a

            Margin a _ ->
                pred a

            MarginLeft a _ ->
                pred a

            MarginRight a _ ->
                pred a

            MarginTop a _ ->
                pred a

            MarginBottom a _ ->
                pred a

            BorderWidth a _ ->
                pred a

            BorderRadius a _ ->
                pred a

            BorderTopLeftRadius a _ ->
                pred a

            BorderTopRightRadius a _ ->
                pred a

            BorderBottomLeftRadius a _ ->
                pred a

            BorderBottomRightRadius a _ ->
                pred a

            LetterSpacing a _ ->
                pred a

            LineHeight a _ ->
                pred a

            BackgroundPosition x y _ ->
                pred x && pred y

            TransformOrigin x y z _ ->
                pred x && pred y && pred z

            Color x y z a ->
                pred x && pred y && pred z && pred a

            BackgroundColor x y z a ->
                pred x && pred y && pred z && pred a

            BorderColor x y z a ->
                pred x && pred y && pred z && pred a

            Translate a1 a2 _ ->
                pred a1 && pred a2

            Translate3d a1 a2 a3 _ ->
                pred a1 && pred a2 && pred a3

            TranslateX a _ ->
                pred a

            TranslateY a _ ->
                pred a

            Scale a1 ->
                pred a1

            Scale3d a1 a2 a3 ->
                pred a1 && pred a2 && pred a3

            ScaleX a ->
                pred a

            ScaleY a ->
                pred a

            ScaleZ a ->
                pred a

            Rotate a _ ->
                pred a

            Rotate3d a1 a2 a3 a4 _ ->
                pred a1 && pred a2 && pred a3 && pred a4

            RotateX a _ ->
                pred a

            RotateY a _ ->
                pred a

            Skew a1 a2 _ ->
                pred a1 && pred a2

            SkewX a _ ->
                pred a

            SkewY a _ ->
                pred a

            Perspective a ->
                pred a

            Matrix a b c x y z ->
                List.all pred [ a, b, c, x, y, z ]

            Matrix3d a b c d e f g h i j k l m n o p ->
                List.all pred [ a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p ]



stepProp : Property a -> Property b -> (Maybe b -> a -> a) -> Property a
stepProp prop prev val =
    case prop of
        Prop name to unit ->
            let
                from =
                    case prev of
                        Prop _ x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                Prop name (val from to) unit

        Display mode ->
            Display mode

        Opacity to ->
            let
                from =
                    case prev of
                        Opacity x ->
                            Just x

                        _ ->
                            Nothing
            in
                Opacity (val from to)

        Height to unit ->
            let
                from =
                    case prev of
                        Height x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                Height (val from to) unit

        Width to unit ->
            let
                from =
                    case prev of
                        Width x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                Width (val from to) unit

        Left to unit ->
            let
                from =
                    case prev of
                        Left x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                Left (val from to) unit

        Top to unit ->
            let
                from =
                    case prev of
                        Top x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                Top (val from to) unit

        Right to unit ->
            let
                from =
                    case prev of
                        Right x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                Right (val from to) unit

        Bottom to unit ->
            let
                from =
                    case prev of
                        Bottom x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                Bottom (val from to) unit

        MaxHeight to unit ->
            let
                from =
                    case prev of
                        MaxHeight x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                MaxHeight (val from to) unit

        MaxWidth to unit ->
            let
                from =
                    case prev of
                        MaxWidth x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                MaxWidth (val from to) unit

        MinHeight to unit ->
            let
                from =
                    case prev of
                        MinHeight x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                MinHeight (val from to) unit

        MinWidth to unit ->
            let
                from =
                    case prev of
                        MinWidth x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                MinWidth (val from to) unit

        Padding to unit ->
            let
                from =
                    case prev of
                        Padding x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                Padding (val from to) unit

        PaddingLeft to unit ->
            let
                from =
                    case prev of
                        PaddingLeft x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                PaddingLeft (val from to) unit

        PaddingRight to unit ->
            let
                from =
                    case prev of
                        PaddingRight x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                PaddingRight (val from to) unit

        PaddingTop to unit ->
            let
                from =
                    case prev of
                        PaddingTop x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                PaddingTop (val from to) unit

        PaddingBottom to unit ->
            let
                from =
                    case prev of
                        PaddingBottom x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                PaddingBottom (val from to) unit

        Margin to unit ->
            let
                from =
                    case prev of
                        Margin x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                Margin (val from to) unit

        MarginLeft to unit ->
            let
                from =
                    case prev of
                        MarginLeft x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                MarginLeft (val from to) unit

        MarginRight to unit ->
            let
                from =
                    case prev of
                        MarginRight x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                MarginRight (val from to) unit

        MarginTop to unit ->
            let
                from =
                    case prev of
                        MarginTop x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                MarginTop (val from to) unit

        MarginBottom to unit ->
            let
                from =
                    case prev of
                        MarginBottom x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                MarginBottom (val from to) unit

        BorderWidth to unit ->
            let
                from =
                    case prev of
                        BorderWidth x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                BorderWidth (val from to) unit

        BorderRadius to unit ->
            let
                from =
                    case prev of
                        BorderRadius x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                BorderRadius (val from to) unit

        BorderTopLeftRadius to unit ->
            let
                from =
                    case prev of
                        BorderTopLeftRadius x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                BorderTopLeftRadius (val from to) unit

        BorderTopRightRadius to unit ->
            let
                from =
                    case prev of
                        BorderTopRightRadius x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                BorderTopRightRadius (val from to) unit

        BorderBottomLeftRadius to unit ->
            let
                from =
                    case prev of
                        BorderBottomLeftRadius x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                BorderBottomLeftRadius (val from to) unit

        BorderBottomRightRadius to unit ->
            let
                from =
                    case prev of
                        BorderBottomRightRadius x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                BorderBottomRightRadius (val from to) unit

        LetterSpacing to unit ->
            let
                from =
                    case prev of
                        LetterSpacing x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                LetterSpacing (val from to) unit

        LineHeight to unit ->
            let
                from =
                    case prev of
                        LineHeight x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                LineHeight (val from to) unit

        BackgroundPosition x y unit ->
            case prev of
                BackgroundPosition xFrom yFrom _ ->
                    BackgroundPosition (val (Just xFrom) x) (val (Just yFrom) y) unit

                _ ->
                    BackgroundPosition (val Nothing x) (val Nothing y) unit

        Color x y z a ->
            let
                ( xFrom, yFrom, zFrom, aFrom ) =
                    case prev of
                        Color x1 y1 z1 a1 ->
                            ( Just x1, Just y1, Just z1, Just a1 )

                        _ ->
                            ( Nothing, Nothing, Nothing, Nothing )
            in
                Color (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a)

        BorderColor x y z a ->
            let
                ( xFrom, yFrom, zFrom, aFrom ) =
                    case prev of
                        BorderColor x1 y1 z1 a1 ->
                            ( Just x1, Just y1, Just z1, Just a1 )

                        _ ->
                            ( Nothing, Nothing, Nothing, Nothing )
            in
                BorderColor (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a)

        BackgroundColor x y z a ->
            let
                ( xFrom, yFrom, zFrom, aFrom ) =
                    case prev of
                        BackgroundColor x1 y1 z1 a1 ->
                            ( Just x1, Just y1, Just z1, Just a1 )

                        _ ->
                            ( Nothing, Nothing, Nothing, Nothing )
            in
                BackgroundColor (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a)

        TransformOrigin x y z unit ->
            let
                ( xFrom, yFrom, zFrom ) =
                    case prev of
                        TransformOrigin x1 y1 z1 _ ->
                            ( Just x1, Just y1, Just z1 )

                        _ ->
                            ( Nothing, Nothing, Nothing )
            in
                TransformOrigin (val xFrom x) (val yFrom y) (val zFrom z) unit

        Translate x y unit ->
            let
                ( xFrom, yFrom ) =
                    case prev of
                        Translate x1 y1 _ ->
                            ( Just x1, Just y1 )

                        _ ->
                            ( Nothing, Nothing )
            in
                Translate (val xFrom x) (val yFrom y) unit

        Translate3d x y z unit ->
            let
                ( xFrom, yFrom, zFrom ) =
                    case prev of
                        Translate3d x1 y1 z1 _ ->
                            ( Just x1, Just y1, Just z1 )

                        _ ->
                            ( Nothing, Nothing, Nothing )
            in
                Translate3d (val xFrom x) (val yFrom y) (val zFrom z) unit

        TranslateX to unit ->
            let
                from =
                    case prev of
                        TranslateX x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                TranslateX (val from to) unit

        TranslateY to unit ->
            let
                from =
                    case prev of
                        TranslateY x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                TranslateY (val from to) unit

        Scale to ->
            let
                from =
                    case prev of
                        Scale x ->
                            Just x

                        _ ->
                            Nothing
            in
                Scale (val from to)

        Scale3d x y z ->
            let
                ( xFrom, yFrom, zFrom ) =
                    case prev of
                        Scale3d x1 y1 z1 ->
                            ( Just x1, Just y1, Just z1 )

                        _ ->
                            ( Nothing, Nothing, Nothing )
            in
                Scale3d (val xFrom x) (val yFrom y) (val zFrom z)

        ScaleX to ->
            let
                from =
                    case prev of
                        ScaleX x ->
                            Just x

                        _ ->
                            Nothing
            in
                ScaleX (val from to)

        ScaleY to ->
            let
                from =
                    case prev of
                        ScaleY x ->
                            Just x

                        _ ->
                            Nothing
            in
                ScaleY (val from to)

        ScaleZ to ->
            let
                from =
                    case prev of
                        ScaleZ x ->
                            Just x

                        _ ->
                            Nothing
            in
                ScaleZ (val from to)

        Rotate to unit ->
            let
                from =
                    case prev of
                        Rotate x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                Rotate (val from to) unit

        Rotate3d x y z a unit ->
            let
                ( xFrom, yFrom, zFrom, aFrom ) =
                    case prev of
                        Rotate3d x1 y1 z1 a1 _ ->
                            ( Just x1, Just y1, Just z1, Just a1 )

                        _ ->
                            ( Nothing, Nothing, Nothing, Nothing )
            in
                Rotate3d (val xFrom x) (val yFrom y) (val zFrom z) (val aFrom a) unit

        RotateX to unit ->
            let
                from =
                    case prev of
                        RotateX x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                RotateX (val from to) unit

        RotateY to unit ->
            let
                from =
                    case prev of
                        RotateY x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                RotateY (val from to) unit

        Skew x y unit ->
            let
                ( xFrom, yFrom ) =
                    case prev of
                        Skew x y _ ->
                            ( Just x, Just y )

                        _ ->
                            ( Nothing, Nothing )
            in
                Skew (val xFrom x) (val yFrom y) unit

        SkewX to unit ->
            let
                from =
                    case prev of
                        SkewX x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                SkewX (val from to) unit

        SkewY to unit ->
            let
                from =
                    case prev of
                        SkewY x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                SkewY (val from to) unit

        Perspective to ->
            let
                from =
                    case prev of
                        SkewY x _ ->
                            Just x

                        _ ->
                            Nothing
            in
                Perspective (val from to)

        Matrix a b c x y z ->
            case prev of
                Matrix aFrom bFrom cFrom xFrom yFrom zFrom ->
                    Matrix (val (Just aFrom) a)
                        (val (Just bFrom) b)
                        (val (Just cFrom) c)
                        (val (Just xFrom) x)
                        (val (Just yFrom) y)
                        (val (Just zFrom) z)

                _ ->
                    Matrix (val Nothing a)
                        (val Nothing b)
                        (val Nothing c)
                        (val Nothing x)
                        (val Nothing y)
                        (val Nothing z)

        Matrix3d a b c d e f g h i j k l m n o p ->
            case prev of
                Matrix3d a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 o2 p2 ->
                    Matrix3d (val (Just a2) a)
                        (val (Just b2) b)
                        (val (Just c2) c)
                        (val (Just d2) d)
                        (val (Just e2) e)
                        (val (Just f2) f)
                        (val (Just g2) g)
                        (val (Just h2) h)
                        (val (Just i2) i)
                        (val (Just j2) j)
                        (val (Just k2) k)
                        (val (Just l2) l)
                        (val (Just m2) m)
                        (val (Just n2) n)
                        (val (Just o2) o)
                        (val (Just p2) p)

                _ ->
                    Matrix3d (val Nothing a)
                        (val Nothing b)
                        (val Nothing c)
                        (val Nothing d)
                        (val Nothing e)
                        (val Nothing f)
                        (val Nothing g)
                        (val Nothing h)
                        (val Nothing i)
                        (val Nothing j)
                        (val Nothing k)
                        (val Nothing l)
                        (val Nothing m)
                        (val Nothing n)
                        (val Nothing o)
                        (val Nothing p)
