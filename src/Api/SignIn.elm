module Api.SignIn exposing (Data, Error, post)

import Effect exposing (Effect)
import Http
import Json.Decode
import Json.Encode


{-| The data we expect if the sign in attempt was successful.
-}
type alias Data =
    { token : String
    }


{-| How to create a `Data` value from JSON
-}
decoder : Json.Decode.Decoder Data
decoder =
    Json.Decode.map Data
        (Json.Decode.field "access_token" Json.Decode.string)


{-| Sends a POST request to our `/api/sign-in` endpoint, which
returns our JWT token if a user was found with that email
and password.
-}
type alias Error =
    { message : String
    , field : Maybe String
    }


post :
    { onResponse :
        Result (List Error) Data
        -> msg
    , email : String
    , password : String
    }
    -> Effect msg
post options =
    let
        body : Json.Encode.Value
        body =
            Json.Encode.object
                [ ( "email", Json.Encode.string options.email )
                , ( "password", Json.Encode.string options.password )
                ]

        cmd : Cmd msg
        cmd =
            Http.post
                { url = "http://localhost:8009/users/generate_token"
                , body = Http.jsonBody body
                , expect =
                    -- Http.expectJson options.onResponse decoder
                    Http.expectStringResponse
                        options.onResponse
                        handleHttpResponse
                }
    in
    Effect.sendCmd cmd


handleHttpResponse : Http.Response String -> Result (List Error) Data
handleHttpResponse response =
    case response of
        Http.BadUrl_ _ ->
            Err
                [ { message = "Formato de URL inesperado"
                  , field = Nothing
                  }
                ]

        Http.Timeout_ ->
            Err
                [ { message = "A solicitação expirou, por favor tente novamente"
                  , field = Nothing
                  }
                ]

        Http.NetworkError_ ->
            Err
                [ { message = "Não foi possível conectar, por favor tente novamente"
                  , field = Nothing
                  }
                ]

        Http.BadStatus_ { statusCode } body ->
            case statusCode of
                401 ->
                    Err
                        [ { message = "Não autorizado: E-mail ou senha inválidos."
                          , field = Nothing
                          }
                        ]

                404 ->
                    Err
                        [ { message = "Não encontrado: O recurso solicitado não pôde ser encontrado."
                          , field = Nothing
                          }
                        ]

                500 ->
                    Err
                        [ { message = "Erro do servidor: Por favor, tente novamente mais tarde."
                          , field = Nothing
                          }
                        ]

                _ ->
                    case Json.Decode.decodeString errorsDecoder body of
                        Ok errors ->
                            Err errors

                        Err _ ->
                            Err
                                [ { message = "Algo inesperado aconteceu"
                                  , field = Nothing
                                  }
                                ]

        Http.GoodStatus_ _ body ->
            case Json.Decode.decodeString decoder body of
                Ok data ->
                    Ok data

                Err _ ->
                    Err
                        [ { message = "Algo inesperado aconteceu"
                          , field = Nothing
                          }
                        ]


errorsDecoder : Json.Decode.Decoder (List Error)
errorsDecoder =
    Json.Decode.field "errors" (Json.Decode.list errorDecoder)


errorDecoder : Json.Decode.Decoder Error
errorDecoder =
    Json.Decode.map2 Error
        (Json.Decode.field "message" Json.Decode.string)
        (Json.Decode.field "field" (Json.Decode.maybe Json.Decode.string))
