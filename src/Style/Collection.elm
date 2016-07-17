module Style.Collection exposing (map3, bake)

import Style.PropertyHelpers exposing (..)


map3 : (Float -> Float -> Physics -> Physics) -> Style -> Style -> List Dynamic -> List Dynamic
map3 fn prev target current =
    let
        matched =
            zipWith3 (\a b -> id a == id b) current prev target
    in
        List.map
            (\( curr, mB, mC ) ->
                case mB of
                    Nothing ->
                        curr

                    Just b ->
                        case mC of
                            Nothing ->
                                curr

                            Just c ->
                                Style.PropertyHelpers.updateOver fn b c curr
            )
            matched


zipWith3 : (a -> b -> Bool) -> List a -> List b -> List b -> List ( a, Maybe b, Maybe b )
zipWith3 fn listA listB listC =
    let
        ( results, _, _ ) =
            List.foldl
                (\a ( stack, bStack, cStack ) ->
                    let
                        ( matchingB, unmatchingB ) =
                            List.partition (\b -> fn a b) bStack

                        remainingB =
                            Maybe.withDefault [] <| List.tail matchingB

                        ( matchingC, unmatchingC ) =
                            List.partition (\c -> fn a c) cStack

                        remainingC =
                            Maybe.withDefault [] <| List.tail matchingC
                    in
                        ( stack ++ [ ( a, List.head matchingB, List.head matchingC ) ]
                        , unmatchingB ++ remainingB
                        , unmatchingC ++ remainingC
                        )
                )
                ( [], listB, listC )
                listA
    in
        results


zipWith : (a -> b -> Bool) -> List a -> List b -> List ( a, Maybe b )
zipWith fn listA listB =
    fst <|
        List.foldl
            (\a ( stack, bStack ) ->
                let
                    ( matching, unmatching ) =
                        List.partition (\b -> fn a b) bStack

                    maybeB =
                        List.head matching

                    remaining =
                        Maybe.withDefault [] <| List.tail matching
                in
                    ( stack ++ [ ( a, maybeB ) ], unmatching ++ remaining )
            )
            ( [], listB )
            listA


fill : Style -> Style -> Style
fill existing new =
    zipWith (\a b -> Style.PropertyHelpers.id a == Style.PropertyHelpers.id b) existing new
        |> List.map (\( a, maybeB ) -> Maybe.withDefault a maybeB)


bake : List Dynamic -> Style -> Style
bake dynamic style =
    fill style <|
        List.map
            toStatic
            dynamic
