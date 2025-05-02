module Api.Letter exposing (..)

import Effect exposing (Effect)
import File
import File.Download
import Http
import Json.Decode exposing (Decoder, andThen, field, map7, string)
import Json.Encode exposing (Value, int, object, string)
import Shared
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



-- letterDecoder : Decoder Letter
-- letterDecoder =
--     Json.Decode.map7 Letter
--         (Json.Decode.field "name" Json.Decode.string)
--         (Json.Decode.field "photo" (Json.Decode.maybe File.decoder))
--         (Json.Decode.field "year_birth" yearBirthDecoder)
--         (Json.Decode.field "year_death" yearDeathDecoder)
--         (Json.Decode.field "date" dateDecoder)
--         (Json.Decode.field "graveyard_name" graveyardNameDecoder)
--         (Json.Decode.field "background" (Json.Decode.maybe File.decoder))
--
--
-- dateDecoder : Decoder EventDate
-- dateDecoder =
--     Json.Decode.map4 EventDate
--         (Json.Decode.field "day" Json.Decode.int)
--         (Json.Decode.field "month" Json.Decode.int)
--         (Json.Decode.field "year" Json.Decode.int)
--         (Json.Decode.field "time" Json.Decode.string)
--
--
-- yearBirthDecoder : Decoder YearBirth
-- yearBirthDecoder =
--     Json.Decode.field "year_birth" Json.Decode.int
--
--
-- yearDeathDecoder : Decoder YearDeath
-- yearDeathDecoder =
--     Json.Decode.field "year_death" Json.Decode.int
--
--
-- graveyardNameDecoder : Decoder GraveyardName
-- graveyardNameDecoder =
--     Json.Decode.field "graveyard_name" Json.Decode.string


letterEncoder : Letter -> Json.Encode.Value
letterEncoder letter =
    Json.Encode.object
        [ ( "name", Json.Encode.string letter.name )
        , ( "year_birth", Json.Encode.int letter.yearBirth )
        , ( "year_death", Json.Encode.int letter.yearDeath )
        , ( "date", dateEncoder letter.date )
        , ( "graveyard_name", Json.Encode.string letter.graveyardName )
        , ( "hour", Json.Encode.int letter.hour )
        , ( "minute", Json.Encode.int letter.minute )
        , ( "hour_end", Json.Encode.int letter.hourEnd )
        , ( "minute_end", Json.Encode.int letter.minuteEnd )
        , ( "wake_name", Json.Encode.string letter.wakeName )
        , ( "two_days", Json.Encode.bool letter.twoDays )
        , ( "no_wake", Json.Encode.bool letter.noWake )
        , ( "no_wake_date", dateEncoder letter.noWakeDate )
        , ( "no_wake_hour", Json.Encode.int letter.noWakeHour )
        , ( "no_wake_minute", Json.Encode.int letter.noWakeMinute )
        , ( "second_date", dateEncoder letter.secondDate )
        , ( "second_hour", Json.Encode.int letter.secondHour )
        , ( "second_minute", Json.Encode.int letter.secondMinute )
        , ( "second_hour_end", Json.Encode.int letter.secondHourEnd )
        , ( "second_minute_end", Json.Encode.int letter.secondMinuteEnd )
        ]


dateEncoder : EventDate -> Json.Encode.Value
dateEncoder date =
    Json.Encode.object
        [ ( "day", Json.Encode.int date.day )
        , ( "month", Json.Encode.int date.month )
        , ( "year", Json.Encode.int date.year )
        ]


fileEncoder : Maybe File.File -> Json.Encode.Value
fileEncoder maybeFile =
    case maybeFile of
        Just file ->
            Json.Encode.object
                [ ( "name", Json.Encode.string (File.name file) )
                , ( "type", Json.Encode.string (File.mime file) )
                , ( "size", Json.Encode.int (File.size file) )

                -- Remove the arrayBuffer field as we can't encode it directly
                -- We'll handle file uploads through multipart/form-data in JavaScript
                ]

        Nothing ->
            Json.Encode.null


post : Letter -> Shared.Model -> Effect String
post letter shared =
    Effect.downloadPdf
        { url = shared.baseUrl ++ "/cartas"
        , letterData = letterEncoder letter
        , files =
            { photo = letter.photo
            , background = letter.background
            }
        , onSuccess = identity
        }


download : String -> Shared.Model -> Effect String
download format shared =
    Effect.downloadImage
        { url = shared.baseUrl ++ "/cartas/" ++ format
        , onSuccess = identity
        }
