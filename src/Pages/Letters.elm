module Pages.Letters exposing (Model, Msg, page)

import Api.Letter
import Effect exposing (Effect)
import Element exposing (..)
import File exposing (File)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Http
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model as Model exposing (..)
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { letterForm : Letter
    , isLoading : Bool
    , errors : List Api.Letter.Error
    , photoUploader : FileUploadModel
    , backgroundUploader : FileUploadModel
    }



-- empty letter form


emptyLetterForm : Letter
emptyLetterForm =
    { name = ""
    , photo = { hover = False, file = Nothing }
    , yearBirth = 0
    , yearDeath = 0
    , date = EventDate 0 0 0 ""
    , graveyardName = ""
    , background = { hover = False, file = Nothing }
    }


init : () -> ( Model, Effect Msg )
init () =
    let
        ( uModel, uCmd ) =
            uploadInit ()
    in
    ( { letterForm = emptyLetterForm
      , isLoading = False
      , errors = []
      , photoUploader = uModel
      , backgroundUploader = uModel
      }
    , Cmd.batch [ Cmd.map PhotoUploaderMsg uCmd, Cmd.map BackgroundUploaderMsg uCmd ]
    )



-- UPDATE


type Msg
    = NoOp
    | PhotoUploaderMsg FileUploadMsg
    | BackgroundUploaderMsg FileUploadMsg


type FileUploadMsg
    = DragEnter
    | DragLeave
    | GotFile File


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )

        PhotoUploaderMsg subMsg ->
            let
                ( uModel, uCmd ) =
                    uploadUpdate subMsg model.photoUploader
            in
            ( { model | photoUploader = uModel }
            , Cmd.map PhotoUploaderMsg uCmd
            )

        BackgroundUploaderMsg subMsg ->
            let
                ( bModel, bCmd ) =
                    uploadUpdate subMsg model.backgroundUploader
            in
            ( { model | backgroundUploader = bModel }
            , Cmd.map BackgroundUploaderMsg bCmd
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Html.form [ Html.Attributes.onSubmit NoOp ]
        [ Html.h2 [] [ Html.text "Letter Information Form" ]
        , Html.div []
            [ Html.label [] [ Html.text "Name:" ]
            , Html.input [ Html.Attributes.type_ "text", Html.Attributes.placeholder "Enter letter name" ] []
            ]
        , Html.div []
            [ Html.label [] [ Html.text "Year of Birth:" ]
            , Html.input [ Html.Attributes.type_ "number", Html.Attributes.placeholder "Year of Birth" ] []
            ]
        , Html.div []
            [ Html.label [] [ Html.text "Year of Death:" ]
            , Html.input [ Html.Attributes.type_ "number", Html.Attributes.placeholder "Year of Death" ] []
            ]
        , Html.div []
            [ Html.label [] [ Html.text "Event Date:" ]
            , Html.input [ Html.Attributes.type_ "date" ] []
            ]
        , Html.div []
            [ Html.label [] [ Html.text "Graveyard Name:" ]
            , Html.input [ Html.Attributes.type_ "text", Html.Attributes.placeholder "Graveyard Name" ] []
            ]
        , Html.h3 [] [ Html.text "Photo Upload" ]
        , uploadView model.photoUploader |> Html.map PhotoUploaderMsg
        , Html.h3 [] [ Html.text "Background Upload" ]
        , uploadView model.backgroundUploader |> Html.map BackgroundUploaderMsg
        , Html.button [ Html.Attributes.type_ "submit" ] [ Html.text "Submit" ]
        ]
