module Api.Letter exposing (..)

-- import Shared exposing (..)

import Effect exposing (Effect)
import File
import Http
import Json.Decode exposing (Decoder, andThen, field, map7, string)
import Shared.Model
    exposing
        ( EventDate
        , GraveyardName
        , Letter
        , YearBirth
        , YearDeath
        )


type alias Error =
    { message : String
    , field : Maybe String
    }


letterDecoder : Decoder Letter
letterDecoder =
    Json.Decode.map7 Letter
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "photo" (Json.Decode.maybe File.decoder))
        (Json.Decode.field "year_birth" yearBirthDecoder)
        (Json.Decode.field "year_death" yearDeathDecoder)
        (Json.Decode.field "date" dateDecoder)
        (Json.Decode.field "graveyard_name" graveyardNameDecoder)
        (Json.Decode.field "background" (Json.Decode.maybe File.decoder))


dateDecoder : Decoder EventDate
dateDecoder =
    Json.Decode.map4 EventDate
        (Json.Decode.field "day" Json.Decode.int)
        (Json.Decode.field "month" Json.Decode.int)
        (Json.Decode.field "year" Json.Decode.int)
        (Json.Decode.field "time" Json.Decode.string)


yearBirthDecoder : Decoder YearBirth
yearBirthDecoder =
    Json.Decode.field "year_birth" Json.Decode.int


yearDeathDecoder : Decoder YearDeath
yearDeathDecoder =
    Json.Decode.field "year_death" Json.Decode.int


graveyardNameDecoder : Decoder GraveyardName
graveyardNameDecoder =
    Json.Decode.field "graveyard_name" Json.Decode.string
