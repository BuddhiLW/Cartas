module Pages.Letters exposing (Model, Msg, page)

import Api.Letter
import Auth
import Components.DatePicker exposing (..)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
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
import View exposing (View)



-- import ImageUpload exposing (update as uploadUpdate, view as uploadView, init as uploadInit)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (toLayout user)


toLayout : Auth.User -> Model -> Layouts.Layout Msg
toLayout user model =
    Layouts.Sidebar
        { title = "Letters"
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
    }



-- empty letter form


emptyLetterForm : Letter
emptyLetterForm =
    Letter
        ""
        Nothing
        0
        0
        (EventDate 0 0 0 "")
        ""
        Nothing


init : () -> ( Model, Effect Msg )
init () =
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
    | DatePickerMsg Components.DatePicker.Msg


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )

        PhotoUploaderMsg subMsg ->
            let
                ( uModel, uCmd ) =
                    ImageUpload.update subMsg model.photoUploader
            in
            ( { model | photoUploader = uModel }
            , Effect.map PhotoUploaderMsg (Effect.sendCmd uCmd)
            )

        BackgroundUploaderMsg subMsg ->
            let
                ( bModel, bCmd ) =
                    ImageUpload.update subMsg model.backgroundUploader
            in
            ( { model | backgroundUploader = bModel }
            , Effect.map BackgroundUploaderMsg (Effect.sendCmd bCmd)
            )

        LetterFieldInput field inputStr ->
            letterFieldInputUpdate model field inputStr

        DatePickerMsg subMsg ->
            let
                ( uModel, uCmd ) =
                    Components.DatePicker.update subMsg model.datePickerModel
            in
            ( { model | datePickerModel = uModel }
            , Effect.map DatePickerMsg (Effect.sendCmd uCmd)
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
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.Letters"
    , attributes = []
    , element = Element.html (viewLetterForm model)
    }


viewLetterForm : Model -> Html Msg
viewLetterForm model =
    Html.form [ Attr.class "box", Html.Events.onSubmit NoOp ]
        [ Html.h2 [ Attr.class "title" ] [ Html.text "Letter Information Form" ]
        , viewLetterFormInput { field = LetterName, value = model.letterForm.name }
        , viewLetterFormInput { field = LetterYearBirth, value = String.fromInt model.letterForm.yearBirth }
        , viewLetterFormInput { field = LetterYearDeath, value = String.fromInt model.letterForm.yearDeath }
        , Html.div [ Attr.style "position" "relative", Attr.style "z-index" "9999" ]
            [ Components.DatePicker.view model.datePickerModel |> Html.map DatePickerMsg ]
        , viewLetterFormInput { field = LetterGraveyardName, value = model.letterForm.graveyardName }
        , Html.div [ Attr.class "field" ]
            [ Html.label [ Attr.class "label" ] [ Html.text "Photo Upload" ]
            , Html.div [ Attr.class "control" ]
                [ ImageUpload.view model.photoUploader |> Html.map PhotoUploaderMsg ]
            ]
        , Html.div [ Attr.class "field" ]
            [ Html.label [ Attr.class "label" ] [ Html.text "Background Upload" ]
            , Html.div [ Attr.class "control" ]
                [ ImageUpload.view model.backgroundUploader |> Html.map BackgroundUploaderMsg ]
            ]
        , viewLetterFormControls
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


viewLetterFormControls : Html Msg
viewLetterFormControls =
    Html.div [ Attr.class "field is-grouped is-grouped-right" ]
        [ Html.div [ Attr.class "control" ]
            [ Html.button [ Attr.class "button is-link", Attr.type_ "submit" ]
                [ Html.text "Submit Letter" ]
            ]
        ]


letterFieldLabel : LetterField -> String
letterFieldLabel field =
    case field of
        LetterName ->
            "Name"

        LetterYearBirth ->
            "Year of Birth"

        LetterYearDeath ->
            "Year of Death"

        LetterDate ->
            "Event Date"

        LetterGraveyardName ->
            "Graveyard Name"


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
    case String.split "-" input of
        [ yearStr, monthStr, dayStr ] ->
            case ( String.toInt yearStr, String.toInt monthStr, String.toInt dayStr ) of
                ( Just y, Just m, Just d ) ->
                    { defaultDate | year = y, month = m, day = d }

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
