module ImageUpload exposing (init, main, update, view)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, preventDefaultOn)
import Json.Decode as D
import Shared.Model exposing (FileUploadModel)



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    FileUploadModel


init : () -> ( Model, Cmd Msg )
init _ =
    ( { hover = False, file = Nothing }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Pick
    | DragEnter
    | DragLeave
    | GotFile File


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pick ->
            ( model
            , Select.files [ "image/*" ]
                (\files ->
                    case files of
                        file :: _ ->
                            GotFile file

                        [] ->
                            DragLeave
                )
            )

        DragEnter ->
            ( { model | hover = True }
            , Cmd.none
            )

        DragLeave ->
            ( { model | hover = False }
            , Cmd.none
            )

        GotFile file ->
            ( { model | file = Just file, hover = False }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "border"
            (if model.hover then
                "6px dashed purple"

             else
                "6px dashed #ccc"
            )
        , style "border-radius" "20px"
        , style "width" "480px"
        , style "height" "100px"
        , style "margin" "100px auto"
        , style "padding" "20px"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "center"
        , style "align-items" "center"
        , hijackOn "dragenter" (D.succeed DragEnter)
        , hijackOn "dragover" (D.succeed DragEnter)
        , hijackOn "dragleave" (D.succeed DragLeave)
        , hijackOn "drop" dropDecoder
        ]
        [ button [ onClick Pick ] [ text "Upload Image" ]
        , span [ style "color" "#ccc" ] [ text (displayFile model.file) ]
        ]


displayFile : Maybe File -> String
displayFile maybeFile =
    case maybeFile of
        Nothing ->
            "No file selected"

        Just file ->
            "File selected: " ++ File.name file


dropDecoder : D.Decoder Msg
dropDecoder =
    D.at [ "dataTransfer", "files" ] (D.list File.decoder)
        |> D.map
            (\files ->
                case files of
                    file :: _ ->
                        GotFile file

                    [] ->
                        DragLeave
            )


hijackOn : String -> D.Decoder msg -> Html.Attribute msg
hijackOn event decoder =
    preventDefaultOn event (D.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )
