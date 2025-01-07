module Api.Role exposing (Role, roleDecoder)

import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value, string)


{-| Role represents the different roles a user can have.
-}
type Role
    = UserRole
    | Admin
    | Attendant


{-| Convert a string to a `Role`.
-}
roleFromString : String -> Maybe Role
roleFromString roleString =
    case roleString of
        "User" ->
            Just UserRole

        "Admin" ->
            Just Admin

        "Attendant" ->
            Just Attendant

        _ ->
            Nothing


{-| Convert a `Role` to a string.
-}
roleToString : Role -> String
roleToString role =
    case role of
        UserRole ->
            "User"

        Admin ->
            "Admin"

        Attendant ->
            "Attendant"


{-| Represents a user.
-}
type alias User =
    { id : String
    , name : String
    , profileImageUrl : String
    , email : String
    , role : Role
    }


{-| Decoder to parse a `Role` from JSON.
-}
roleDecoder : Json.Decode.Decoder Role
roleDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\roleString ->
                case roleFromString roleString of
                    Just role ->
                        Json.Decode.succeed role

                    Nothing ->
                        Json.Decode.fail ("Invalid role: " ++ roleString)
            )
