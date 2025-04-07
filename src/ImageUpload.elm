module ImageUpload exposing (Model, Msg, init, main, update, view)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, preventDefaultOn)
import Json.Decode as D
import Maybe exposing (withDefault)
import Shared.Model exposing (FileUpload(..), FileUploadModel, FileUploadType(..))
import Task



-- MAIN


main =
    Browser.element
        { init = init Photo
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    FileUploadModel


init : FileUploadType -> () -> ( Model, Cmd Msg )
init fileType _ =
    ( { hover = False, file = Nothing, fileType = fileType }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Pick
    | DragEnter
    | DragLeave
    | GotFiles File (List File)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pick ->
            ( model
            , Select.files [ "image/*" ] GotFiles
            )

        DragEnter ->
            ( { model | hover = True }
            , Cmd.none
            )

        DragLeave ->
            ( { model | hover = False }
            , Cmd.none
            )

        GotFiles _ _ ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model.file of
        Nothing ->
            viewUpload

        Just file ->
            viewPreview ""


viewUpload : Html Msg
viewUpload =
    div
        [ onClick Pick
        , hijackOn "dragenter" (D.succeed DragEnter)
        , hijackOn "dragleave" (D.succeed DragLeave)
        , hijackOn "drop" dropDecoder
        , style "border" "2px dashed #ccc"
        , style "padding" "20px"
        , style "text-align" "center"
        , style "cursor" "pointer"
        ]
        [ text "Click or drag an image here to upload" ]


viewPreview : String -> Html msg
viewPreview url =
    div
        [ style "width" "60px"
        , style "height" "60px"
        , style "background-image" ("url('" ++ url ++ "')")
        , style "background-position" "center"
        , style "background-repeat" "no-repeat"
        , style "background-size" "contain"
        ]
        []


dropDecoder : D.Decoder Msg
dropDecoder =
    D.at [ "dataTransfer", "files" ] (D.oneOrMore GotFiles File.decoder)


hijackOn : String -> D.Decoder msg -> Html.Attribute msg
hijackOn event decoder =
    preventDefaultOn event (D.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )



-- Helper function to determine if the file is a photo


isPhoto : File -> Bool
isPhoto file =
    -- Implement logic to check the file type (e.g., by extension or MIME type)
    True



-- Placeholder logic, replace with actual implementation
