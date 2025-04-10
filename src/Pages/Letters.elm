module Pages.Letters exposing (Model, Msg, page)

import Api.Letter
import Auth
import Components.DatePicker exposing (..)
import Effect exposing (Effect)
import Element exposing (..)
import File exposing (File)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import ImageUpload exposing (..)
import Layouts exposing (Layout)
import Page exposing (Page)
import Route exposing (Route)
import Shared exposing (..)
import Shared.Model exposing (..)
import Task
import View exposing (View)



-- import ImageUpload exposing (update as uploadUpdate, view as uploadView, init as uploadInit)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init shared
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (toLayout user)


toLayout : Auth.User -> Model -> Layouts.Layout Msg
toLayout user model =
    Layouts.Sidebar
        { title = "Formulário de Nota de Falecimento"
        , user = user
        }



-- INIT


type alias Model =
    { letterForm : Letter
    , isLoading : Bool
    , errors : List Api.Letter.Error
    , photoUploader : ImageUpload.Model
    , backgroundUploader : ImageUpload.Model
    , datePickerModel : Components.DatePicker.Model
    , pdfUrl : Maybe String
    , shared : Shared.Model
    }



-- empty letter form


type alias Letter =
    { name : String
    , photo : Maybe File
    , yearBirth : Int
    , yearDeath : Int
    , date : EventDate
    , graveyardName : String
    , background : Maybe File
    , hour : Int
    , minute : Int
    }


emptyLetterForm : Letter
emptyLetterForm =
    Letter
        ""
        Nothing
        0
        0
        (EventDate 0 0 0)
        ""
        Nothing
        19
        0



-- Default minute


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    let
        ( photoUploaderModel, photoUploaderCmd ) =
            ImageUpload.init Photo ()

        ( backgroundUploaderModel, backgroundUploaderCmd ) =
            ImageUpload.init Background ()

        ( datePickerModel, datePickerCmd ) =
            Components.DatePicker.init
    in
    ( { letterForm = emptyLetterForm
      , isLoading = False
      , errors = []
      , photoUploader = photoUploaderModel
      , backgroundUploader = backgroundUploaderModel
      , datePickerModel = datePickerModel
      , pdfUrl = Nothing
      , shared = shared
      }
    , Effect.batch
        [ Effect.map PhotoUploaderMsg (Effect.sendCmd photoUploaderCmd)
        , Effect.map BackgroundUploaderMsg (Effect.sendCmd backgroundUploaderCmd)
        , Effect.map DatePickerMsg (Effect.sendCmd datePickerCmd)
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | PhotoUploaderMsg ImageUpload.Msg
    | BackgroundUploaderMsg ImageUpload.Msg
    | LetterFieldInput LetterField String
    | HourInput String
    | MinuteInput String
    | DatePickerMsg Components.DatePicker.Msg
    | GotPdfUrl String
    | DownloadPdf String
    | SubmitForm


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )

        PhotoUploaderMsg subMsg ->
            let
                --                 _ =
                --                     Debug.log "PhotoUploaderMsg" subMsg
                ( updatedUploader, uploaderCmd ) =
                    ImageUpload.update subMsg model.photoUploader

                --                 _ =
                --                     Debug.log "Updated uploader state"
                --                         { file = updatedUploader.file
                --                         , type_ = updatedUploader.fileType
                --                         }
                lf =
                    model.letterForm

                updatedLetterForm =
                    case updatedUploader.file of
                        Just (FileUpload _ file) ->
                            { lf | photo = Just file }

                        Nothing ->
                            model.letterForm

                newModel =
                    { model
                        | photoUploader = updatedUploader
                        , letterForm = updatedLetterForm
                    }

                --                 _ =
                --                     Debug.log "Final model state"
                --                         { photoUploaderFile = newModel.photoUploader.file
                --                         , letterFormPhoto = newModel.letterForm.photo
                --                         }
            in
            ( newModel
            , Effect.map PhotoUploaderMsg (Effect.sendCmd uploaderCmd)
            )

        BackgroundUploaderMsg subMsg ->
            let
                ( bModel, bCmd ) =
                    ImageUpload.update subMsg model.backgroundUploader

                lf =
                    model.letterForm

                newLetterForm =
                    case bModel.file of
                        Just (FileUpload _ file) ->
                            { lf | background = Just file }

                        Nothing ->
                            lf
            in
            ( { model
                | backgroundUploader = bModel
                , letterForm = newLetterForm
              }
            , Effect.map BackgroundUploaderMsg (Effect.sendCmd bCmd)
            )

        LetterFieldInput field inputStr ->
            letterFieldInputUpdate model field inputStr

        HourInput inputStr ->
            let
                hour =
                    String.toInt inputStr
                        |> Maybe.withDefault model.letterForm.hour
                        |> clamp 0 23
            in
            ( { model | letterForm = setHour hour model.letterForm }
            , Effect.none
            )

        MinuteInput inputStr ->
            let
                minute =
                    String.toInt inputStr
                        |> Maybe.withDefault model.letterForm.minute
                        |> clamp 0 59
            in
            ( { model | letterForm = setMinute minute model.letterForm }
            , Effect.none
            )

        DatePickerMsg subMsg ->
            let
                ( uModel, uCmd ) =
                    Components.DatePicker.update subMsg model.datePickerModel

                -- Extract date from picker model
                newDate =
                    case uModel.date of
                        Just date ->
                            let
                                ( day, month, year ) =
                                    parseDateFromPicker uModel.dateText
                            in
                            EventDate day month year

                        Nothing ->
                            model.letterForm.date
            in
            ( { model
                | datePickerModel = uModel
                , letterForm = setDate newDate model.letterForm
              }
            , Effect.map DatePickerMsg (Effect.sendCmd uCmd)
            )

        GotPdfUrl url ->
            ( { model
                | isLoading = False
                , pdfUrl = Just url
              }
            , Effect.sendCmd (Task.perform identity (Task.succeed (DownloadPdf url)))
            )

        DownloadPdf url ->
            ( { model | isLoading = False }
            , Effect.none
            )

        SubmitForm ->
            ( { model | isLoading = True }
            , Effect.map GotPdfUrl (Api.Letter.post model.letterForm model.shared)
            )


letterFieldInputUpdate : Model -> LetterField -> String -> ( Model, Effect Msg )
letterFieldInputUpdate model field inputStr =
    let
        lf =
            model.letterForm
    in
    case field of
        LetterName ->
            ( { model | letterForm = { lf | name = inputStr } }
            , Effect.none
            )

        LetterYearBirth ->
            ( { model
                | letterForm =
                    { lf
                        | yearBirth =
                            case String.toInt inputStr of
                                Just n ->
                                    n

                                Nothing ->
                                    model.letterForm.yearBirth
                    }
              }
            , Effect.none
            )

        LetterYearDeath ->
            ( { model
                | letterForm =
                    { lf
                        | yearDeath =
                            case String.toInt inputStr of
                                Just n ->
                                    n

                                Nothing ->
                                    model.letterForm.yearDeath
                    }
              }
            , Effect.none
            )

        LetterDate ->
            ( { model
                | letterForm =
                    { lf
                        | date = parseDate inputStr lf.date
                    }
              }
            , Effect.none
            )

        LetterGraveyardName ->
            ( { model | letterForm = { lf | graveyardName = inputStr } }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Effect.receivePdfUrl GotPdfUrl



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.Letters"
    , attributes = []
    , element = Element.html (viewLetterForm model)
    }


viewLetterForm : Model -> Html Msg
viewLetterForm model =
    Html.form [ Attr.class "box is-centered is-fullwidth", Html.Events.onSubmit NoOp ]
        [ Html.h2 [ Attr.class "title" ] [ Html.text "Preencha todos os dados para gerar a nota de falecimento" ]
        , viewLetterFormInput { field = LetterName, value = model.letterForm.name }
        , viewLetterFormInput { field = LetterYearBirth, value = String.fromInt model.letterForm.yearBirth }
        , viewLetterFormInput { field = LetterYearDeath, value = String.fromInt model.letterForm.yearDeath }
        , Html.div
            [ Attr.class "columns is-vcentered is-mobile"
            , Attr.style "z-index" "999"
            ]
            [ Html.div
                [ Attr.class "column is-half"
                , Attr.style "z-index" "999"
                ]
                [ Components.DatePicker.view model.datePickerModel |> Html.map DatePickerMsg ]
            , Html.div
                [ Attr.class "column is-half"
                , Attr.style "z-index" "999"
                ]
                [ viewTimeInput model
                ]
            ]
        , viewLetterFormInput { field = LetterGraveyardName, value = model.letterForm.graveyardName }
        , Html.div [ Attr.class "field" ]
            [ Html.label [ Attr.class "label" ] [ Html.text "Foto do Perfil" ]
            , Html.div [ Attr.class "control" ]
                [ ImageUpload.view model.photoUploader |> Html.map PhotoUploaderMsg ]
            ]
        , Html.div [ Attr.class "field" ]
            [ Html.label [ Attr.class "label" ] [ Html.text "Imagem de Fundo da Nota" ]
            , Html.div [ Attr.class "control" ]
                [ ImageUpload.view model.backgroundUploader |> Html.map BackgroundUploaderMsg ]
            ]
        , viewLetterFormControls model
        ]


viewLetterFormInput : { field : LetterField, value : String } -> Html Msg
viewLetterFormInput { field, value } =
    Html.div [ Attr.class "field" ]
        [ Html.label [ Attr.class "label" ] [ Html.text (letterFieldLabel field) ]
        , Html.div [ Attr.class "control" ]
            [ Html.input
                [ Attr.class "input"
                , Attr.type_ (letterFieldInputType field)
                , Attr.value value
                , Html.Events.onInput (LetterFieldInput field)
                ]
                []
            ]
        ]


viewLetterFormControls : Model -> Html Msg
viewLetterFormControls model =
    Html.div [ Attr.class "field is-grouped is-grouped-right" ]
        [ Html.div [ Attr.class "control" ]
            [ Html.button
                [ Attr.class "button is-link is-large"
                , Attr.type_ "button"
                , Html.Events.onClick SubmitForm
                , Attr.disabled model.isLoading
                ]
                [ Html.text
                    (if model.isLoading then
                        "Gerando..."

                     else
                        --  (" ++ String.fromInt model.letterForm.hour ++ "h" ++ String.fromInt model.letterForm.minute ++ "m)"
                        "Gerar Nota"
                    )
                ]
            ]
        ]


viewTimeInput : Model -> Html Msg
viewTimeInput model =
    Html.div []
        [ Html.label [ Attr.class "label" ] [ Html.text "Horário" ]
        , Html.div [ Attr.class "field has-addons" ]
            [ Html.div [ Attr.class "control" ]
                [ Html.input
                    [ Attr.class "input"
                    , Attr.type_ "number"
                    , Attr.placeholder "Hora"
                    , Attr.value (String.fromInt model.letterForm.hour)
                    , Html.Events.onInput HourInput
                    , Attr.min "0"
                    , Attr.max "23"
                    ]
                    []
                ]
            , Html.div [ Attr.class "control" ]
                [ Html.a [ Attr.class "button is-static" ] [ Html.text "h" ] ]
            , Html.div [ Attr.class "control" ]
                [ Html.input
                    [ Attr.class "input"
                    , Attr.type_ "number"
                    , Attr.placeholder "Minuto"
                    , Attr.value (String.fromInt model.letterForm.minute)
                    , Html.Events.onInput MinuteInput
                    , Attr.min "0"
                    , Attr.max "59"
                    ]
                    []
                ]
            , Html.div [ Attr.class "control" ]
                [ Html.a [ Attr.class "button is-static" ] [ Html.text "m" ] ]
            ]
        ]


letterFieldLabel : LetterField -> String
letterFieldLabel field =
    case field of
        LetterName ->
            "Nome"

        LetterYearBirth ->
            "Ano de Nascimento"

        LetterYearDeath ->
            "Ano de Falecimento"

        LetterDate ->
            "Data do Enterro"

        LetterGraveyardName ->
            "Nome do Cemitério"


letterFieldInputType : LetterField -> String
letterFieldInputType field =
    case field of
        LetterName ->
            "text"

        LetterYearBirth ->
            "number"

        LetterYearDeath ->
            "number"

        LetterDate ->
            "date"

        LetterGraveyardName ->
            "text"


formatDate : EventDate -> String
formatDate eventDate =
    let
        pad n =
            if n < 10 then
                "0" ++ String.fromInt n

            else
                String.fromInt n
    in
    String.fromInt eventDate.year ++ "-" ++ pad eventDate.month ++ "-" ++ pad eventDate.day


parseDate : String -> EventDate -> EventDate
parseDate input defaultDate =
    case String.split "/" input of
        [ dayStr, monthStr, yearStr ] ->
            case ( String.toInt yearStr, String.toInt monthStr, String.toInt dayStr ) of
                ( Just y, Just m, Just d ) ->
                    { defaultDate | day = d, month = m, year = y }

                _ ->
                    defaultDate

        _ ->
            defaultDate



-- Add this type definition at the appropriate place in your file, likely near the top with other type definitions


type LetterField
    = LetterName
    | LetterYearBirth
    | LetterYearDeath
    | LetterDate
    | LetterGraveyardName


setHour : Int -> Letter -> Letter
setHour hour letter =
    { letter | hour = hour }


setMinute : Int -> Letter -> Letter
setMinute minute letter =
    { letter | minute = minute }


setDate : EventDate -> Letter -> Letter
setDate date letter =
    { letter | date = date }


parseDateFromPicker : String -> ( Int, Int, Int )
parseDateFromPicker dateText =
    case String.split "/" dateText of
        [ dayStr, monthStr, yearStr ] ->
            ( Maybe.withDefault 0 (String.toInt dayStr)
            , Maybe.withDefault 0 (String.toInt monthStr)
            , Maybe.withDefault 0 (String.toInt yearStr)
            )

        _ ->
            ( 0, 0, 0 )
