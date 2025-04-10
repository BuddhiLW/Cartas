module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    )

{-|

@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions

-}

import Api.Role
import Dict
import Effect exposing (Effect)
import Json.Decode
import Route exposing (Route)
import Route.Path
import Shared.Model
import Shared.Msg



-- FLAGS


type alias Flags =
    { serviceBaseUrl : String
    , user : Maybe Shared.Model.User
    }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.map2 Flags
        (Json.Decode.field "serviceBaseUrl" Json.Decode.string)
        (Json.Decode.field "user" (Json.Decode.maybe userDecoder))


userDecoder : Json.Decode.Decoder Shared.Model.User
userDecoder =
    Json.Decode.map6 Shared.Model.User
        (Json.Decode.field "token" Json.Decode.string)
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "image" Json.Decode.string)
        (Json.Decode.field "email" Json.Decode.string)
        (Json.Decode.field "role" Json.Decode.string)



-- INIT


type alias Model =
    { baseUrl : String
    , user : Maybe Shared.Model.User
    }


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult _ =
    let
        flags =
            case flagsResult of
                Ok f ->
                    f

                Err _ ->
                    { serviceBaseUrl = "http://localhost:8009"
                    , user = Nothing
                    }
    in
    ( { baseUrl = flags.serviceBaseUrl
      , user = flags.user
      }
    , Effect.none
    )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        Shared.Msg.SignIn user ->
            ( { model | user = Just user }
            , Effect.batch
                [ Effect.pushRoute
                    { path = Route.Path.Letters
                    , query = Dict.empty
                    , hash = Nothing
                    }
                , Effect.saveUser user
                ]
            )

        Shared.Msg.SignOut ->
            ( { model | user = Nothing }
            , Effect.clearUser
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.none
