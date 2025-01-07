module Api.Me exposing (User, get)

import Api.Role
import Effect exposing (Effect)
import Http
import Json.Decode exposing (Decoder, andThen, field, map5, string)


type alias User =
    { id : String
    , name : String
    , image : String
    , email : String
    , role : String
    }


{-| Decoder to parse a `User` from JSON.
-}
userDecoder : Decoder User
userDecoder =
    Json.Decode.map5 User
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "image" Json.Decode.string)
        (Json.Decode.field "email" Json.Decode.string)
        (Json.Decode.field "role" Json.Decode.string)


{-| Fetch the currently authenticated user (`/user`).
-}
get :
    { onResponse : Result Http.Error User -> msg
    , token : String
    }
    -> Effect msg
get options =
    let
        url : String
        url =
            "http://localhost:8009/user"

        headers : List Http.Header
        headers =
            [ Http.header "Authorization" ("Bearer " ++ options.token)
            , Http.header "Content-Type" "application/json"

            -- , Http.header "User-Agent" "Emacs"
            ]

        cmd : Cmd msg
        cmd =
            Http.request
                { method = "GET"
                , headers = headers
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectJson options.onResponse userDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd
