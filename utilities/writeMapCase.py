import string
types = """
Display DisplayMode
Opacity a
Height a Length
Width a Length
Left a Length
Top a Length
Right a Length
Bottom a Length
MaxHeight a Length
MaxWidth a Length
MinHeight a Length
MinWidth a Length
Padding a Length
PaddingLeft a Length
PaddingRight a Length
PaddingTop a Length
PaddingBottom a Length
Margin a Length
MarginLeft a Length
MarginRight a Length
MarginTop a Length
MarginBottom a Length
BorderWidth a Length
BorderRadius a Length
BorderTopLeftRadius a Length
BorderTopRightRadius a Length
BorderBottomLeftRadius a Length
BorderBottomRightRadius a Length
LetterSpacing a Length
LineHeight a Length
BackgroundPosition a a Length
Color a a a a
BackgroundColor a a a a
BorderColor a a a a
TransformOrigin a a a Length
Matrix a a a a a a
Matrix3d a a a a a a a a a a a a a a a a
Translate a a Length
Translate3d a a a Length
TranslateX a Length
TranslateY a Length
Scale a
Scale3d a a a
ScaleX a
ScaleY a
ScaleZ a
Rotate a Angle
Rotate3d a a a a Angle
RotateX a Angle
RotateY a Angle
Skew a a Angle
SkewX a Angle
SkewY a Angle
Perspective a
"""


svg = """
X a
Y a
Cx a
Cy a
R a
Rx a
Ry a
D a
Points list
Width a
Height a
"""


cmds = """
Move a a
MoveTo a a
Line a a
LineTo a a
Horizontal a
HorizontalTo a
Vertical a
VerticalTo a
Curve points
CurveTo points
Quadratic points
QuadraticTo points
SmoothQuadratic points
SmoothQuadraticTo points
Smooth points
SmoothTo points
Arc a a a a
ArcTo a a a a
LargeArc a a a a
LargeArcTo a a a a
Close
"""


map3Template = """{name} {props1} ->
    case cmd2 of
        {name} {props2} ->
            case cmd3 of
                {name} {props3} ->
                    {name} {fnprops}
                _ -> cmd3
        _ -> cmd3
"""

for entity in cmds.strip().split("\n"):
  components = entity.split()
  name = components[0]
  props = []
  ids = string.ascii_lowercase[:26]
  for comp in components[1:]:
    if comp == "a":
      props.append(ids[len(props)])
    if comp == "points":
       props.append("points")
    elif comp == "Length":
      props.append("unit")
    elif comp == "Angle":
      props.append("unit")
    elif comp == "DisplayMode":
      props.append("mode")
    elif comp == "list":
      props.append("props")


  props1 = [c + "1" for c in props]

  props2 = [c + "2" for c in props]

  props3 = [c + "3" for c in props]

  fnprops = []
  for p1, p2, p3 in zip(props1, props2, props3):
    if p1 == "unit1":
      fnprops.append("unit1")
    elif p1 == "points":
      fnprops.append((" ".join(["(map3Points fn", p1, p2, p3]) + ")"  ))
    else:
      fnprops.append((" ".join(["(fn", p1, p2, p3]) + ")"  ))
  # fnprops = [c + "2" for c in props]
  # print(fnprops)
  fnprops = " ".join(fnprops)
  props1 = " ".join(props1)
  props2 = " ".join(props2)
  props3 = " ".join(props3)

  case = map3Template.format(**{"name": name, "props1": props1, "props2": props2, "props3":props3, "fnprops":fnprops})
  print(case)
