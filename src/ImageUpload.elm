module ImageUpload exposing (Model, Msg, init, main, update, view)

import Browser
import Debug
import Effect exposing (Effect, storeFile)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, preventDefaultOn)
import Json.Decode as D
import Json.Encode as E
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
    , previews : List String
    }


init : FileUploadType -> () -> ( Model, Cmd Msg )
init fileType _ =
    ( { hover = False
      , file = Nothing
      , fileType = fileType
      , previews = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Pick
    | DragEnter
    | DragLeave
    | GotFiles File (List File)
    | GotPreviews (List String)
    | RemoveFile
    | NoOp
    | StoreFileUrl String String


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

        GotFiles first rest ->
            ( { model
                | file = Just (FileUpload model.fileType first)
                , hover = False
              }
            , Cmd.batch
                [ File.toUrl first
                    |> Task.perform (List.singleton >> GotPreviews)
                , Task.map2 Tuple.pair (Task.succeed (File.name first)) (File.toUrl first)
                    |> Task.perform (\( name, url ) -> StoreFileUrl name url)
                ]
            )

        GotPreviews urls ->
            ( { model | previews = urls }, Cmd.none )

        RemoveFile ->
            ( { model
                | file = Nothing
                , previews = []
                , hover = False
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )

        StoreFileUrl name url ->
            ( model
            , Effect.storeFile
                { name = name
                , file = E.string url
                }
            )



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

        Just (FileUpload _ _) ->
            -- Use the first preview URL from our list
            case List.head model.previews of
                Just url ->
                    viewPreview url

                Nothing ->
                    -- Show loading state or fallback while waiting for preview
                    viewPreview "loading..."


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


viewPreview : String -> Html Msg
viewPreview url =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "gap" "8px"
        ]
        [ div
            [ style "width" "100px"
            , style "height" "100px"
            , style "background-size" "cover"
            , style "background-position" "center"
            , style "border-radius" "4px"
            , style "border" "1px solid #ccc"
            , style "background-image" ("url('" ++ url ++ "')")
            ]
            []
        , div
            [ style "display" "flex"
            , style "gap" "8px"
            ]
            [ button
                [ onClick Pick
                , style "padding" "4px 8px"
                , style "border-radius" "4px"
                , style "border" "1px solid #ccc"
                ]
                [ text "Trocar" ]
            , button
                [ onClick RemoveFile
                , style "padding" "4px 8px"
                , style "border-radius" "4px"
                , style "border" "1px solid #ccc"
                , style "color" "#cc0000"
                ]
                [ text "Remover" ]
            ]
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
