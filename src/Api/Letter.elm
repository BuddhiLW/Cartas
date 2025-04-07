module Api.Letter exposing (..)

-- import Shared exposing (..)

import Effect exposing (Effect)
import File
import Http
import Json.Decode exposing (Decoder, andThen, field, map7, string)
import Shared.Model
    exposing
        ( Background
        , DateBirth
        , EventDate
        , GraveyardName
        , Letter
        , Photo
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
        (Json.Decode.field "photo" photoDecoder)
        (Json.Decode.field "year_birth" yearBirthDecoder)
        (Json.Decode.field "year_death" yearDeathDecoder)
        (Json.Decode.field "date" dateDecoder)
        (Json.Decode.field "graveyard_name" graveyardNameDecoder)
        (Json.Decode.field "background" backgroundDecoder)


dateDecoder : Decoder EventDate
dateDecoder =
    Json.Decode.map4 EventDate
        (Json.Decode.field "day" Json.Decode.int)
        (Json.Decode.field "month" Json.Decode.int)
        (Json.Decode.field "year" Json.Decode.int)
        (Json.Decode.field "time" Json.Decode.string)


photoDecoder : Decoder Photo
photoDecoder =
    Json.Decode.field "photo" File.decoder


backgroundDecoder : Decoder Background
backgroundDecoder =
    Json.Decode.field "background" File.decoder


yearBirthDecoder : Decoder YearBirth
yearBirthDecoder =
    Json.Decode.field "year_birth" Json.Decode.int


yearDeathDecoder : Decoder YearDeath
yearDeathDecoder =
    Json.Decode.field "year_death" Json.Decode.int


dateBirthDecoder : Decoder DateBirth
dateBirthDecoder =
    Json.Decode.map3 DateBirth
        (Json.Decode.field "year" Json.Decode.int)
        (Json.Decode.field "month" Json.Decode.int)
        (Json.Decode.field "day" Json.Decode.int)


graveyardNameDecoder : Decoder GraveyardName
graveyardNameDecoder =
    Json.Decode.field "graveyard_name" Json.Decode.string
