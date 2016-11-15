module Main exposing (..)

import Html exposing (h1, div, Html)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Animation
import Color exposing (black, rgb)


type alias Model =
    { style : Animation.State
    , index : Int
    }


type Msg
    = Morph
    | Animate Animation.Msg


type alias BatSignal =
    List Animation.PathStep


startLogo : BatSignal
startLogo =
    [ Animation.moveTo 212 220
    , Animation.curve2To
        { control1 = ( 197, 171 )
        , control2 = ( 156, 153 )
        , point = ( 123, 221 )
        }
    , Animation.curve2To
        { control1 = ( 109, 157 )
        , control2 = ( 120, 109 )
        , point = ( 159, 63.6 )
        }
    , Animation.curve2To
        { control1 = ( 190, 114 )
        , control2 = ( 234, 115 )
        , point = ( 254, 89.8 )
        }
    , Animation.curve2To
        { control1 = ( 260, 82.3 )
        , control2 = ( 268, 69.6 )
        , point = ( 270, 60.3 )
        }
    , Animation.curve2To
        { control1 = ( 273, 66.5 )
        , control2 = ( 275, 71.6 )
        , point = ( 280, 75.6 )
        }
    , Animation.curve2To
        { control1 = ( 286, 79.5 )
        , control2 = ( 294, 79.8 )
        , point = ( 300, 79.8 )
        }
    , Animation.curve2To
        { control1 = ( 306, 79.8 )
        , control2 = ( 314, 79.5 )
        , point = ( 320, 75.6 )
        }
    , Animation.curve2To
        { control1 = ( 325, 71.6 )
        , control2 = ( 327, 66.5 )
        , point = ( 330, 60.3 )
        }
    , Animation.curve2To
        { control1 = ( 332, 69.6 )
        , control2 = ( 340, 82.3 )
        , point = ( 346, 89.8 )
        }
    , Animation.curve2To
        { control1 = ( 366, 115 )
        , control2 = ( 410, 114 )
        , point = ( 441, 63.6 )
        }
    , Animation.curve2To
        { control1 = ( 480, 109 )
        , control2 = ( 491, 157 )
        , point = ( 477, 221 )
        }
    , Animation.curve2To
        { control1 = ( 444, 153 )
        , control2 = ( 403, 171 )
        , point = ( 388, 220 )
        }
    , Animation.curve2To
        { control1 = ( 366, 188 )
        , control2 = ( 316, 200 )
        , point = ( 300, 248 )
        }
    , Animation.curve2To
        { control1 = ( 284, 200 )
        , control2 = ( 234, 188 )
        , point = ( 212, 220 )
        }
    , Animation.close
    ]


batmanLogos : List BatSignal
batmanLogos =
    [ [ Animation.moveTo 256 213
      , Animation.curve2To
            { control1 = ( 245, 181 )
            , control2 = ( 206, 187 )
            , point = ( 234, 262 )
            }
      , Animation.curve2To
            { control1 = ( 147, 181 )
            , control2 = ( 169, 71.2 )
            , point = ( 233, 18 )
            }
      , Animation.curve2To
            { control1 = ( 220, 56 )
            , control2 = ( 235, 81 )
            , point = ( 283, 88 )
            }
      , Animation.curve2To
            { control1 = ( 285, 78.7 )
            , control2 = ( 286, 69.3 )
            , point = ( 288, 60 )
            }
      , Animation.curve2To
            { control1 = ( 289, 61.3 )
            , control2 = ( 290, 62.7 )
            , point = ( 291, 64 )
            }
      , Animation.curve2To
            { control1 = ( 291, 64 )
            , control2 = ( 297, 63 )
            , point = ( 300, 63 )
            }
      , Animation.curve2To
            { control1 = ( 303, 63 )
            , control2 = ( 309, 64 )
            , point = ( 309, 64 )
            }
      , Animation.curve2To
            { control1 = ( 310, 62.7 )
            , control2 = ( 311, 61.3 )
            , point = ( 312, 60 )
            }
      , Animation.curve2To
            { control1 = ( 314, 69.3 )
            , control2 = ( 315, 78.7 )
            , point = ( 317, 88 )
            }
      , Animation.curve2To
            { control1 = ( 365, 82 )
            , control2 = ( 380, 56 )
            , point = ( 367, 18 )
            }
      , Animation.curve2To
            { control1 = ( 431, 71 )
            , control2 = ( 453, 181 )
            , point = ( 366, 262 )
            }
      , Animation.curve2To
            { control1 = ( 394, 187 )
            , control2 = ( 356, 181 )
            , point = ( 344, 213 )
            }
      , Animation.curve2To
            { control1 = ( 328, 185 )
            , control2 = ( 309, 184 )
            , point = ( 300, 284 )
            }
      , Animation.curve2To
            { control1 = ( 291, 184 )
            , control2 = ( 272, 185 )
            , point = ( 256, 213 )
            }
      , Animation.close
      ]
    , [ Animation.moveTo 212 220
      , Animation.curve2To
            { control1 = ( 197, 171 )
            , control2 = ( 156, 153 )
            , point = ( 123, 221 )
            }
      , Animation.curve2To
            { control1 = ( 109, 157 )
            , control2 = ( 120, 109 )
            , point = ( 159, 63.6 )
            }
      , Animation.curve2To
            { control1 = ( 190, 114 )
            , control2 = ( 234, 115 )
            , point = ( 254, 89.8 )
            }
      , Animation.curve2To
            { control1 = ( 260, 82.3 )
            , control2 = ( 268, 69.6 )
            , point = ( 270, 60.3 )
            }
      , Animation.curve2To
            { control1 = ( 273, 66.5 )
            , control2 = ( 275, 71.6 )
            , point = ( 280, 75.6 )
            }
      , Animation.curve2To
            { control1 = ( 286, 79.5 )
            , control2 = ( 294, 79.8 )
            , point = ( 300, 79.8 )
            }
      , Animation.curve2To
            { control1 = ( 306, 79.8 )
            , control2 = ( 314, 79.5 )
            , point = ( 320, 75.6 )
            }
      , Animation.curve2To
            { control1 = ( 325, 71.6 )
            , control2 = ( 327, 66.5 )
            , point = ( 330, 60.3 )
            }
      , Animation.curve2To
            { control1 = ( 332, 69.6 )
            , control2 = ( 340, 82.3 )
            , point = ( 346, 89.8 )
            }
      , Animation.curve2To
            { control1 = ( 366, 115 )
            , control2 = ( 410, 114 )
            , point = ( 441, 63.6 )
            }
      , Animation.curve2To
            { control1 = ( 480, 109 )
            , control2 = ( 491, 157 )
            , point = ( 477, 221 )
            }
      , Animation.curve2To
            { control1 = ( 444, 153 )
            , control2 = ( 403, 171 )
            , point = ( 388, 220 )
            }
      , Animation.curve2To
            { control1 = ( 366, 188 )
            , control2 = ( 316, 200 )
            , point = ( 300, 248 )
            }
      , Animation.curve2To
            { control1 = ( 284, 200 )
            , control2 = ( 234, 188 )
            , point = ( 212, 220 )
            }
      , Animation.close
      ]
    , [ Animation.moveTo 213 222
      , Animation.curve2To
            { control1 = ( 219, 150 )
            , control2 = ( 165, 139 )
            , point = ( 130, 183 )
            }
      , Animation.curve2To
            { control1 = ( 125, 123 )
            , control2 = ( 171, 73.8 )
            , point = ( 247, 51.6 )
            }
      , Animation.curve2To
            { control1 = ( 205, 78 )
            , control2 = ( 236, 108 )
            , point = ( 280, 102 )
            }
      , Animation.curve2To
            { control1 = ( 281, 90.3 )
            , control2 = ( 282, 79 )
            , point = ( 286, 68.2 )
            }
      , Animation.curve2To
            { control1 = ( 287, 72 )
            , control2 = ( 288, 75.8 )
            , point = ( 289, 79.7 )
            }
      , Animation.curve2To
            { control1 = ( 293, 79.7 )
            , control2 = ( 296, 79.7 )
            , point = ( 300, 79.7 )
            }
      , Animation.curve2To
            { control1 = ( 304, 79.7 )
            , control2 = ( 307, 79.7 )
            , point = ( 311, 79.7 )
            }
      , Animation.curve2To
            { control1 = ( 312, 75.8 )
            , control2 = ( 313, 72 )
            , point = ( 314, 68.2 )
            }
      , Animation.curve2To
            { control1 = ( 318, 79 )
            , control2 = ( 319, 90.3 )
            , point = ( 320, 102 )
            }
      , Animation.curve2To
            { control1 = ( 364, 108 )
            , control2 = ( 395, 78 )
            , point = ( 353, 51.6 )
            }
      , Animation.curve2To
            { control1 = ( 429, 73.8 )
            , control2 = ( 475, 123 )
            , point = ( 470, 183 )
            }
      , Animation.curve2To
            { control1 = ( 435, 139 )
            , control2 = ( 381, 150 )
            , point = ( 387, 222 )
            }
      , Animation.curve2To
            { control1 = ( 364, 176 )
            , control2 = ( 315, 172 )
            , point = ( 300, 248 )
            }
      , Animation.curve2To
            { control1 = ( 285, 172 )
            , control2 = ( 236, 176 )
            , point = ( 213, 222 )
            }
      , Animation.close
      ]
    , [ Animation.moveTo 218 231
      , Animation.curve2To
            { control1 = ( 191, 238 )
            , control2 = ( 165, 252 )
            , point = ( 140, 266 )
            }
      , Animation.curve2To
            { control1 = ( 144, 209 )
            , control2 = ( 156, 153 )
            , point = ( 193, 93.7 )
            }
      , Animation.curve2To
            { control1 = ( 218, 106 )
            , control2 = ( 249, 105 )
            , point = ( 280, 102 )
            }
      , Animation.curve2To
            { control1 = ( 282, 90.3 )
            , control2 = ( 284, 78.6 )
            , point = ( 289, 67.8 )
            }
      , Animation.curve2To
            { control1 = ( 290, 71.6 )
            , control2 = ( 291, 75.8 )
            , point = ( 292, 79.7 )
            }
      , Animation.curve2To
            { control1 = ( 292, 79.7 )
            , control2 = ( 297, 79.7 )
            , point = ( 300, 79.7 )
            }
      , Animation.curve2To
            { control1 = ( 303, 79.7 )
            , control2 = ( 308, 79.7 )
            , point = ( 308, 79.7 )
            }
      , Animation.curve2To
            { control1 = ( 309, 75.8 )
            , control2 = ( 310, 71.6 )
            , point = ( 311, 67.8 )
            }
      , Animation.curve2To
            { control1 = ( 316, 78.6 )
            , control2 = ( 318, 90.3 )
            , point = ( 320, 102 )
            }
      , Animation.curve2To
            { control1 = ( 351, 105 )
            , control2 = ( 382, 106 )
            , point = ( 407, 93.7 )
            }
      , Animation.curve2To
            { control1 = ( 444, 153 )
            , control2 = ( 456, 209 )
            , point = ( 460, 266 )
            }
      , Animation.curve2To
            { control1 = ( 435, 252 )
            , control2 = ( 409, 238 )
            , point = ( 382, 231 )
            }
      , Animation.curve2To
            { control1 = ( 355, 224 )
            , control2 = ( 328, 223 )
            , point = ( 300, 223 )
            }
      , Animation.curve2To
            { control1 = ( 272, 223 )
            , control2 = ( 245, 224 )
            , point = ( 218, 231 )
            }
      , Animation.close
      ]
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Morph ->
            let
                wrappedIndex =
                    if List.length batmanLogos < model.index then
                        model.index - List.length batmanLogos
                    else
                        model.index

                newPath =
                    Maybe.withDefault startLogo <|
                        List.head <|
                            (List.drop wrappedIndex batmanLogos)
                                ++ (List.take wrappedIndex batmanLogos)
            in
                ( { model
                    | index = wrappedIndex + 1
                    , style =
                        Animation.interrupt
                            [ Animation.to
                                [ Animation.path newPath ]
                            ]
                            model.style
                  }
                , Cmd.none
                )

        Animate time ->
            ( { model
                | style = Animation.update time model.style
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div
        [ onClick Morph
        , Attr.style [ ( "margin", "200px auto" ), ( "width", "1000px" ), ( "height", "1000px" ), ( "cursor", "pointer" ) ]
        ]
        [ h1 [] [ text "Click to morph!" ]
        , svg
            [ version "1.1"
            , x "0"
            , y "0"
            , viewBox "0 0 1000 1000"
            ]
            [ Svg.path (Animation.render model.style) []
            ]
        ]


init : ( Model, Cmd Msg )
init =
    ( { style =
            Animation.style
                [ Animation.fill black
                , Animation.path startLogo
                ]
      , index = 2
      }
    , Cmd.none
    )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\model -> Animation.subscription Animate [ model.style ])
        }
