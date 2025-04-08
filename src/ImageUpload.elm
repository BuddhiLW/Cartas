module ImageUpload exposing (Model, Msg, init, main, update, view)

import Browser
import Debug
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
    { hover : Bool
    , file : Maybe FileUpload -- This should match FileUploadModel
    , fileType : FileUploadType
    }


init : FileUploadType -> () -> ( Model, Cmd Msg )
init fileType _ =
    ( { hover = False
      , file = Nothing
      , fileType = fileType
      }
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

        GotFiles firstFile remainingFiles ->
            let
                _ =
                    Debug.log "GotFiles called with"
                        { first = firstFile
                        , rest = remainingFiles
                        }

                newModel =
                    { model
                        | file = Just (FileUpload model.fileType firstFile)
                        , hover = False
                    }

                _ =
                    Debug.log "Updated model" newModel
            in
            ( newModel, Cmd.none )

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
            viewUpload model

        Just (FileUpload _ file) ->
            viewPreview (File.name file)


viewUpload : Model -> Html Msg
viewUpload model =
    div
        [ onClick Pick
        , hijackOn "dragenter" (D.succeed DragEnter)
        , hijackOn "dragover" (D.succeed DragEnter)
        , hijackOn "dragleave" (D.succeed DragLeave)
        , hijackOn "drop" dropDecoder
        , style "border" "2px dashed #ccc"
        , style "border-radius" "8px"
        , style "padding" "20px"
        , style "text-align" "center"
        , style "cursor" "pointer"
        , style "transition" "all 0.3s ease"
        , style "border-color"
            (if model.hover then
                "#666"

             else
                "#ccc"
            )
        ]
        [ text "Clique ou arraste uma imagem aqui para fazer o upload" ]


viewPreview : String -> Html msg
viewPreview fileName =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "gap" "8px"
        ]
        [ div
            [ style "width" "100px"
            , style "height" "100px"
            , style "background" "#f0f0f0"
            , style "border-radius" "4px"
            , style "padding" "8px"
            ]
            [ text "Preview will show here" ]
        , span
            [ style "font-size" "0.8em" ]
            [ text fileName ]
        ]


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
