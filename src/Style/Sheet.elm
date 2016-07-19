module Style.Sheet exposing (Model, tick, render, attrs, update, init)

import Style
import Style.Properties
import Color
import Svg


type alias Model id msg =
    List ( id, Style.Animation msg )


tick : Float -> Model id msg -> ( Model id msg, List msg )
tick time sheet =
    List.foldl
        (\( class, style ) ( styles, messages ) ->
            let
                ( updated, msgs ) =
                    Style.tick time style
            in
                ( styles ++ [ ( class, updated ) ]
                , messages ++ msgs
                )
        )
        ( [], [] )
        sheet


render : Model id msg -> id -> List ( String, String )
render sheet id =
    let
        matching =
            List.filter (\x -> fst x == id) sheet
    in
        case List.head matching of
            Nothing ->
                []

            Just style ->
                Style.render <| snd style


attrs : Model id msg -> id -> List (Svg.Attribute msg)
attrs sheet id =
    let
        matching =
            List.filter (\x -> fst x == id) sheet
    in
        case List.head matching of
            Nothing ->
                []

            Just style ->
                Style.attrs <| snd style


update : (id -> Style.Animation msg -> Style.Animation msg) -> Model id msg -> Model id msg
update fn sheet =
    List.map
        (\( id, x ) ->
            ( id, fn id x )
        )
        sheet


initWith : Style.Options -> (id -> List (Style.Properties.Property Float Color.Color)) -> List id -> Model id msg
initWith options initFn ids =
    List.map
        (\id ->
            ( id, Style.initWith options <| initFn id )
        )
        ids


init : (id -> List (Style.Properties.Property Float Color.Color)) -> List id -> Model id msg
init initFn ids =
    List.map
        (\id ->
            ( id, Style.init <| initFn id )
        )
        ids
