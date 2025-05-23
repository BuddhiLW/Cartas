module Shared.Msg exposing (Msg(..))

{-| -}


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type Msg
    = SignIn
        { token : String
        , id : String
        , name : String
        , image : String
        , email : String
        , role : String
        }
    | SignOut
