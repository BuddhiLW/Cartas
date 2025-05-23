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
    , secondDatePickerModel : Components.DatePicker.Model
    , noWakeDatePickerModel : Components.DatePicker.Model
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
    , hourEnd : Int
    , minuteEnd : Int
    , wakeName : String
    , twoDays : Bool
    , noWake : Bool
    , noWakeDate : EventDate
    , noWakeHour : Int
    , noWakeMinute : Int
    , secondDate : EventDate
    , secondHour : Int
    , secondMinute : Int
    , secondHourEnd : Int
    , secondMinuteEnd : Int
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
        23
        59
        ""
        False
        False
        (EventDate 0 0 0)
        -1
        -1
        (EventDate 0 0 0)
        -1
        -1
        -1
        -1



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

        ( secondDatePickerModel, secondDatePickerCmd ) =
            Components.DatePicker.init

        ( noWakeDatePickerModelInit, noWakeDatePickerCmd ) =
            Components.DatePicker.init

        noWakeDatePickerModel =
            { noWakeDatePickerModelInit | label = "Data do Sepultamento" }
    in
    ( { letterForm = emptyLetterForm
      , isLoading = False
      , errors = []
      , photoUploader = photoUploaderModel
      , backgroundUploader = backgroundUploaderModel
      , datePickerModel = datePickerModel
      , pdfUrl = Nothing
      , shared = shared
      , secondDatePickerModel = secondDatePickerModel
      , noWakeDatePickerModel = noWakeDatePickerModel
      }
    , Effect.batch
        [ Effect.map PhotoUploaderMsg (Effect.sendCmd photoUploaderCmd)
        , Effect.map BackgroundUploaderMsg (Effect.sendCmd backgroundUploaderCmd)
        , Effect.map DatePickerMsg (Effect.sendCmd datePickerCmd)
        , Effect.map SecondDatePickerMsg (Effect.sendCmd secondDatePickerCmd)
        , Effect.map NoWakeDatePickerMsg (Effect.sendCmd noWakeDatePickerCmd)
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
    | HourEndInput String
    | MinuteEndInput String
    | WakeNameInput String
    | DatePickerMsg Components.DatePicker.Msg
    | GotPdfUrl String
    | GotImageUrl String
    | DownloadPdf String
    | SubmitForm
    | TwoDaysInput Bool
    | NoWakeInput Bool
    | SecondDatePickerMsg Components.DatePicker.Msg
    | SecondHourInput String
    | SecondMinuteInput String
    | SecondHourEndInput String
    | SecondMinuteEndInput String
    | NoWakeDatePickerMsg Components.DatePicker.Msg
    | NoWakeHourInput String
    | NoWakeMinuteInput String


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
            if inputStr == "" then
                ( { model | letterForm = setHour -1 model.letterForm }
                , Effect.none
                )

            else
                case String.toInt inputStr of
                    Just n ->
                        ( { model | letterForm = setHour (clamp 0 23 n) model.letterForm }
                        , Effect.none
                        )

                    Nothing ->
                        ( model, Effect.none )

        MinuteInput inputStr ->
            if inputStr == "" then
                ( { model | letterForm = setMinute -1 model.letterForm }
                , Effect.none
                )

            else
                case String.toInt inputStr of
                    Just n ->
                        ( { model | letterForm = setMinute (clamp 0 59 n) model.letterForm }
                        , Effect.none
                        )

                    Nothing ->
                        ( model, Effect.none )

        HourEndInput inputStr ->
            let
                lf =
                    model.letterForm
            in
            if inputStr == "" then
                ( { model | letterForm = { lf | hourEnd = -1 } }
                , Effect.none
                )

            else
                case String.toInt inputStr of
                    Just n ->
                        ( { model | letterForm = { lf | hourEnd = clamp 0 23 n } }
                        , Effect.none
                        )

                    Nothing ->
                        ( model, Effect.none )

        MinuteEndInput inputStr ->
            let
                lf =
                    model.letterForm
            in
            if inputStr == "" then
                ( { model | letterForm = { lf | minuteEnd = -1 } }
                , Effect.none
                )

            else
                case String.toInt inputStr of
                    Just n ->
                        ( { model | letterForm = { lf | minuteEnd = clamp 0 59 n } }
                        , Effect.none
                        )

                    Nothing ->
                        ( model, Effect.none )

        WakeNameInput inputStr ->
            let
                lf =
                    model.letterForm
            in
            ( { model | letterForm = { lf | wakeName = inputStr } }
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
            , Effect.batch
                [ Effect.sendCmd (Task.perform identity (Task.succeed (DownloadPdf url)))
                , Api.Letter.download "jpeg" model.shared |> Effect.map GotImageUrl
                ]
            )

        GotImageUrl imgUrl ->
            ( model, Effect.none )

        DownloadPdf url ->
            ( { model | isLoading = False }
            , Effect.none
            )

        SubmitForm ->
            ( { model | isLoading = True }
            , Effect.map GotPdfUrl (Api.Letter.post model.letterForm model.shared)
            )

        TwoDaysInput isChecked ->
            let
                lf =
                    model.letterForm
            in
            ( { model
                | letterForm = { lf | twoDays = isChecked }
              }
            , Effect.none
            )

        NoWakeInput isChecked ->
            let
                lf =
                    model.letterForm
            in
            ( { model | letterForm = { lf | noWake = isChecked } }
            , Effect.none
            )

        NoWakeDatePickerMsg subMsg ->
            let
                ( updatedDPModel, dpCmd ) =
                    Components.DatePicker.update subMsg model.noWakeDatePickerModel

                newNoWakeDate =
                    case updatedDPModel.date of
                        Just _ ->
                            let
                                ( day, month, year ) =
                                    parseDateFromPicker updatedDPModel.dateText
                            in
                            EventDate day month year

                        Nothing ->
                            model.letterForm.noWakeDate

                lf =
                    model.letterForm
            in
            ( { model
                | noWakeDatePickerModel = updatedDPModel
                , letterForm = { lf | noWakeDate = newNoWakeDate }
              }
            , Effect.map NoWakeDatePickerMsg (Effect.sendCmd dpCmd)
            )

        NoWakeHourInput inputStr ->
            let
                lf =
                    model.letterForm
            in
            if inputStr == "" then
                ( { model | letterForm = { lf | noWakeHour = -1 } }
                , Effect.none
                )

            else
                case String.toInt inputStr of
                    Just n ->
                        ( { model | letterForm = { lf | noWakeHour = clamp 0 23 n } }
                        , Effect.none
                        )

                    Nothing ->
                        ( model, Effect.none )

        NoWakeMinuteInput inputStr ->
            let
                lf =
                    model.letterForm
            in
            if inputStr == "" then
                ( { model | letterForm = { lf | noWakeMinute = -1 } }
                , Effect.none
                )

            else
                case String.toInt inputStr of
                    Just n ->
                        ( { model | letterForm = { lf | noWakeMinute = clamp 0 59 n } }
                        , Effect.none
                        )

                    Nothing ->
                        ( model, Effect.none )

        SecondDatePickerMsg subMsg ->
            let
                ( updatedDPModel, dpCmd ) =
                    Components.DatePicker.update subMsg model.secondDatePickerModel

                newSecondDate =
                    case updatedDPModel.date of
                        Just _ ->
                            let
                                ( day, month, year ) =
                                    parseDateFromPicker updatedDPModel.dateText
                            in
                            EventDate day month year

                        Nothing ->
                            model.letterForm.secondDate

                lf =
                    model.letterForm
            in
            ( { model
                | secondDatePickerModel = updatedDPModel
                , letterForm = { lf | secondDate = newSecondDate }
              }
            , Effect.map SecondDatePickerMsg (Effect.sendCmd dpCmd)
            )

        SecondHourInput inputStr ->
            let
                lf =
                    model.letterForm
            in
            if inputStr == "" then
                ( { model | letterForm = { lf | secondHour = -1 } }
                , Effect.none
                )

            else
                case String.toInt inputStr of
                    Just n ->
                        ( { model | letterForm = { lf | secondHour = clamp 0 23 n } }
                        , Effect.none
                        )

                    Nothing ->
                        ( model, Effect.none )

        SecondMinuteInput inputStr ->
            let
                lf =
                    model.letterForm
            in
            if inputStr == "" then
                ( { model | letterForm = { lf | secondMinute = -1 } }
                , Effect.none
                )

            else
                case String.toInt inputStr of
                    Just n ->
                        ( { model | letterForm = { lf | secondMinute = clamp 0 59 n } }
                        , Effect.none
                        )

                    Nothing ->
                        ( model, Effect.none )

        SecondHourEndInput inputStr ->
            let
                lf =
                    model.letterForm
            in
            if inputStr == "" then
                ( { model | letterForm = { lf | secondHourEnd = -1 } }
                , Effect.none
                )

            else
                case String.toInt inputStr of
                    Just n ->
                        ( { model | letterForm = { lf | secondHourEnd = clamp 0 23 n } }
                        , Effect.none
                        )

                    Nothing ->
                        ( model, Effect.none )

        SecondMinuteEndInput inputStr ->
            let
                lf =
                    model.letterForm
            in
            if inputStr == "" then
                ( { model | letterForm = { lf | secondMinuteEnd = -1 } }
                , Effect.none
                )

            else
                case String.toInt inputStr of
                    Just n ->
                        ( { model | letterForm = { lf | secondMinuteEnd = clamp 0 59 n } }
                        , Effect.none
                        )

                    Nothing ->
                        ( model, Effect.none )


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
        , Html.div [ Attr.class "columns is-vcentered is-mobile" ]
            [ Html.div
                [ Attr.class "column is-one-third"
                , Attr.classList [ ( "is-hidden", model.letterForm.noWake ) ]
                ]
                [ Html.label [ Attr.class "checkbox" ]
                    [ Html.input
                        [ Attr.type_ "checkbox"
                        , Attr.checked model.letterForm.twoDays
                        , Html.Events.onCheck TwoDaysInput
                        ]
                        []
                    , Html.text " Incluir segunda data"
                    ]
                ]
            , Html.div [ Attr.class "field column is-one-third" ]
                [ Html.label [ Attr.class "checkbox" ]
                    [ Html.input
                        [ Attr.type_ "checkbox"
                        , Attr.checked model.letterForm.noWake
                        , Html.Events.onCheck NoWakeInput
                        ]
                        []
                    , Html.text " Sepultamento direto"
                    ]
                ]
            ]
        , Html.div
            [ Attr.class "columns is-vcentered is-mobile"
            , Attr.classList [ ( "is-hidden", model.letterForm.noWake ) ]
            , Attr.style "z-index" "999"
            ]
            [ Html.div
                [ Attr.class "column is-one-third"
                , Attr.style "z-index" "999"
                ]
                [ Components.DatePicker.view model.datePickerModel |> Html.map DatePickerMsg ]
            , Html.div
                [ Attr.class "column is-one-third"
                , Attr.style "z-index" "999"
                ]
                [ viewTimeInput model ]
            , Html.div
                [ Attr.class "column is-one-third"
                , Attr.style "z-index" "999"
                ]
                [ viewTimeEndInput model ]
            ]
        , if model.letterForm.twoDays then
            Html.div
                [ Attr.class "columns is-vcentered is-mobile mt-5"
                , Attr.style "z-index" "998"
                , Attr.classList [ ( "is-hidden", model.letterForm.noWake ) ]
                ]
                [ Html.div
                    [ Attr.class "column is-one-third"
                    , Attr.style "z-index" "998"
                    ]
                    [ Components.DatePicker.view model.secondDatePickerModel |> Html.map SecondDatePickerMsg ]
                , Html.div
                    [ Attr.class "column is-one-third"
                    , Attr.style "z-index" "998"
                    ]
                    [ viewSecondTimeInput model ]
                , Html.div
                    [ Attr.class "column is-one-third"
                    , Attr.style "z-index" "998"
                    ]
                    [ viewSecondTimeEndInput model ]
                ]

          else
            Html.text ""
        , if model.letterForm.noWake then
            Html.div
                [ Attr.class "columns is-vcentered is-mobile"
                , Attr.style "z-index" "998"
                ]
                [ Html.div
                    [ Attr.class "column is-one-third"
                    , Attr.style "z-index" "998"
                    ]
                    [ Components.DatePicker.view model.noWakeDatePickerModel |> Html.map NoWakeDatePickerMsg ]
                , Html.div
                    [ Attr.class "column is-one-third"
                    , Attr.style "z-index" "998"
                    ]
                    [ viewNoWakeTimeInput model ]
                ]

          else
            Html.text ""
        , viewWakeNameInput model
        , viewLetterFormInput { field = LetterGraveyardName, value = model.letterForm.graveyardName }
        , Html.div [ Attr.class "field" ]
            [ Html.label [ Attr.class "label" ] [ Html.text "Foto do Perfil" ]
            , Html.div [ Attr.class "control" ]
                [ case model.photoUploader.file of
                    Just (FileUpload _ file) ->
                        ImageUpload.view model.photoUploader |> Html.map PhotoUploaderMsg

                    Nothing ->
                        Html.div [ Attr.class "flex flex-row" ]
                            [ ImageUpload.view model.photoUploader |> Html.map PhotoUploaderMsg
                            , Html.div [ Attr.class "flex flex-row" ]
                                [ Html.p [ Attr.class "is-size-12 mt-3" ] [ Html.text "Caso não selecione uma foto, esta sairá padrão:" ]
                                , Html.img [ Attr.class "image is-64x64", Attr.src "/luto.png", Attr.alt "Default Profile" ] []
                                ]
                            ]
                ]
            ]
        , Html.div [ Attr.class "field" ]
            [ Html.label [ Attr.class "label" ] [ Html.text "Imagem de Fundo da Nota" ]
            , Html.div [ Attr.class "control" ]
                [ case model.backgroundUploader.file of
                    Just (FileUpload _ file) ->
                        ImageUpload.view model.backgroundUploader |> Html.map BackgroundUploaderMsg

                    Nothing ->
                        Html.div [ Attr.class "flex is-flex-row" ]
                            [ ImageUpload.view model.backgroundUploader |> Html.map BackgroundUploaderMsg
                            , Html.div [ Attr.class "flex flex-row" ]
                                [ Html.p [ Attr.class "is-size-12 mt-3" ] [ Html.text "Caso não selecione uma imagem de fundo, esta sairá padrão:" ]
                                , Html.img [ Attr.class "image is-64x64", Attr.src "/fundo2.png", Attr.alt "Default Background" ] []
                                ]
                            ]
                ]
            ]
        , viewLetterFormControls model
        ]


viewSecondTimeInput : Model -> Html Msg
viewSecondTimeInput model =
    Html.div []
        [ Html.label [ Attr.class "label" ] [ Html.text "Horário (Segunda)" ]
        , Html.div [ Attr.class "field has-addons" ]
            [ Html.div [ Attr.class "control" ]
                [ Html.input
                    [ Attr.class "input"
                    , Attr.type_ "number"
                    , Attr.placeholder "Hora"
                    , Attr.value
                        (if model.letterForm.secondHour == -1 then
                            ""

                         else
                            String.fromInt model.letterForm.secondHour
                        )
                    , Html.Events.onInput SecondHourInput
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
                    , Attr.value
                        (if model.letterForm.secondMinute == -1 then
                            ""

                         else
                            String.fromInt model.letterForm.secondMinute
                        )
                    , Html.Events.onInput SecondMinuteInput
                    , Attr.min "0"
                    , Attr.max "59"
                    ]
                    []
                ]
            , Html.div [ Attr.class "control" ]
                [ Html.a [ Attr.class "button is-static" ] [ Html.text "m" ] ]
            ]
        ]


viewNoWakeTimeInput : Model -> Html Msg
viewNoWakeTimeInput model =
    Html.div []
        [ Html.label [ Attr.class "label" ] [ Html.text "Horário" ]
        , Html.div [ Attr.class "field has-addons" ]
            [ Html.div [ Attr.class "control" ]
                [ Html.input
                    [ Attr.class "input"
                    , Attr.type_ "number"
                    , Attr.placeholder "Hora"
                    , Attr.value
                        (if model.letterForm.noWakeHour == -1 then
                            ""

                         else
                            String.fromInt model.letterForm.noWakeHour
                        )
                    , Html.Events.onInput NoWakeHourInput
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
                    , Attr.value
                        (if model.letterForm.noWakeMinute == -1 then
                            ""

                         else
                            String.fromInt model.letterForm.noWakeMinute
                        )
                    , Html.Events.onInput NoWakeMinuteInput
                    , Attr.min "0"
                    , Attr.max "59"
                    ]
                    []
                ]
            , Html.div [ Attr.class "control" ]
                [ Html.a [ Attr.class "button is-static" ] [ Html.text "m" ] ]
            ]
        ]


viewSecondTimeEndInput : Model -> Html Msg
viewSecondTimeEndInput model =
    Html.div []
        [ Html.label [ Attr.class "label" ] [ Html.text "Horário Fim (Segunda)" ]
        , Html.div [ Attr.class "field has-addons" ]
            [ Html.div [ Attr.class "control" ]
                [ Html.input
                    [ Attr.class "input"
                    , Attr.type_ "number"
                    , Attr.placeholder "Hora fim"
                    , Attr.value
                        (if model.letterForm.secondHourEnd == -1 then
                            ""

                         else
                            String.fromInt model.letterForm.secondHourEnd
                        )
                    , Html.Events.onInput SecondHourEndInput
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
                    , Attr.placeholder "Minuto fim"
                    , Attr.value
                        (if model.letterForm.secondMinuteEnd == -1 then
                            ""

                         else
                            String.fromInt model.letterForm.secondMinuteEnd
                        )
                    , Html.Events.onInput SecondMinuteEndInput
                    , Attr.min "0"
                    , Attr.max "59"
                    ]
                    []
                ]
            , Html.div [ Attr.class "control" ]
                [ Html.a [ Attr.class "button is-static" ] [ Html.text "m" ] ]
            ]
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
                    , Attr.value
                        (if model.letterForm.hour == -1 then
                            ""

                         else
                            String.fromInt model.letterForm.hour
                        )
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
                    , Attr.value
                        (if model.letterForm.minute == -1 then
                            ""

                         else
                            String.fromInt model.letterForm.minute
                        )
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


viewTimeEndInput : Model -> Html Msg
viewTimeEndInput model =
    Html.div []
        [ Html.label [ Attr.class "label" ] [ Html.text "Horário Fim" ]
        , Html.div [ Attr.class "field has-addons" ]
            [ Html.div [ Attr.class "control" ]
                [ Html.input
                    [ Attr.class "input"
                    , Attr.type_ "number"
                    , Attr.placeholder "Hora fim"
                    , Attr.value
                        (if model.letterForm.hourEnd == -1 then
                            ""

                         else
                            String.fromInt model.letterForm.hourEnd
                        )
                    , Html.Events.onInput HourEndInput
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
                    , Attr.placeholder "Minuto fim"
                    , Attr.value
                        (if model.letterForm.minuteEnd == -1 then
                            ""

                         else
                            String.fromInt model.letterForm.minuteEnd
                        )
                    , Html.Events.onInput MinuteEndInput
                    , Attr.min "0"
                    , Attr.max "59"
                    ]
                    []
                ]
            , Html.div [ Attr.class "control" ]
                [ Html.a [ Attr.class "button is-static" ] [ Html.text "m" ] ]
            ]
        ]


viewWakeNameInput : Model -> Html Msg
viewWakeNameInput model =
    Html.div
        [ Attr.class "field"
        , Attr.classList [ ( "is-hidden", model.letterForm.noWake ) ]
        ]
        [ Html.label [ Attr.class "label" ] [ Html.text "Nome do Velório" ]
        , Html.div
            [ Attr.class "control"
            ]
            [ Html.input
                [ Attr.class "input"
                , Attr.type_ "text"
                , Attr.value model.letterForm.wakeName
                , Html.Events.onInput WakeNameInput
                ]
                []
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
