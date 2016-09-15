module Animation.Messenger exposing (State, update, send)

{-| Import this module if you want the ability to send a Msg at any point during your animation.

@docs State, update, send

-}

import Animation.Model exposing (..)


{-| An Animation State that also tracks your `Msg` type.
-}
type alias State msg =
    Animation msg


{-| An update that returns the updated animation as well as any messages sent in `Cmd` form.
-}
update : Tick -> Animation msg -> ( Animation msg, Cmd msg )
update tick animation =
    updateAnimation tick animation


{-| An animation `Step` which will send a message.  For example

    Animation.interrupt
        [ Animation.to [Animation.opacity 0]
        , Animation.send OpacityIsNotZero
        ]


-}
send : msg -> Step msg
send msg =
    Send msg
