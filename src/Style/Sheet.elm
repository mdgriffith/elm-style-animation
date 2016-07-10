module Style.Sheet exposing (..) --where

import Style.Properties

type Sheet a = 
        List (Count a)





type Count a
    = One a Style
    | Many (Int -> a) (Int -> Style)



on : undefined



tick : undefined