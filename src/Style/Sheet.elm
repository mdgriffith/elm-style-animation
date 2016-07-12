module Style.Sheet exposing (Model, tick, render, attrs, update, init)

import Style
import Style.Properties
import Color
import Svg


type alias Model id =
    List ( id, Style.Animation )


tick : Float -> Model id -> Model id
tick time sheet =
    List.map (\( i, x ) -> ( i, Style.tick time x )) sheet


render : Model id -> id -> List ( String, String )
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


attrs : Model id -> id -> List (Svg.Attribute msg)
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


update : Model id -> (id -> Style.Animation -> Style.Animation) -> Model id
update sheet fn =
    List.map
        (\( id, x ) ->
            ( id, fn id x )
        )
        sheet


init : List id -> (id -> List (Style.Properties.Property Float Color.Color)) -> Model id
init ids initFn =
    List.map
        (\id ->
            ( id, Style.init <| initFn id )
        )
        ids
