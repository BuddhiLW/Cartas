module Components.DatePicker exposing (..)

import Browser
import Date exposing (Date)
import DatePicker exposing (ChangeEvent(..))
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Maybe.Extra
import Task
import Time exposing (Month(..), Weekday(..))



-- MODEL
-- MODEL


type alias Model =
    { date : Maybe Date
    , dateText : String
    , pickerModel : DatePicker.Model
    }



-- MESSAGES


type Msg
    = ChangePicker ChangeEvent
    | SetToday Date
    | OnFocus
    | OnLoseFocus
    | OnClick
    | OnDoubleClick
    | OnMouseDown
    | OnMouseUp
    | OnMouseEnter
    | OnMouseLeave
    | OnMouseMove



-- INITIALIZATION


init : ( Model, Cmd Msg )
init =
    ( { date = Nothing
      , dateText = ""
      , pickerModel = DatePicker.init
      }
    , Task.perform SetToday Date.today
    )



-- VIEW


view : Model -> Html Msg
view model =
    DatePicker.input
        [ Element.width (Element.px 180)
        , Element.centerX
        , Element.centerY
        , Element.padding 42
        , Background.color (Element.rgb255 100 200 255)
        , Border.color (Element.rgb255 0 100 200)
        , Border.rounded 5
        , Events.onFocus OnFocus
        , Events.onLoseFocus OnLoseFocus
        , Events.onClick OnClick
        , Events.onDoubleClick OnDoubleClick
        , Events.onMouseDown OnMouseDown
        , Events.onMouseUp OnMouseUp
        , Events.onMouseEnter OnMouseEnter
        , Events.onMouseLeave OnMouseLeave
        , Events.onMouseMove OnMouseMove
        ]
        { onChange = ChangePicker
        , selected = model.date
        , text = model.dateText
        , label = Input.labelAbove [] <| Element.text "Data do Enterro"
        , placeholder = Just <| Input.placeholder [] <| Element.text "Dia/Mês/Ano"
        , settings = settings
        , model = model.pickerModel
        }
        |> Element.layout []



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePicker changeEvent ->
            case changeEvent of
                DateChanged date ->
                    ( { model
                        | date = Just date
                        , dateText = Date.toIsoString date
                        , pickerModel = model.pickerModel |> DatePicker.close
                      }
                    , Cmd.none
                    )

                TextChanged text ->
                    ( { model
                        | date = Date.fromIsoString text |> Result.toMaybe |> Maybe.Extra.orElse model.date
                        , dateText = text
                      }
                    , Cmd.none
                    )

                PickerChanged subMsg ->
                    ( { model | pickerModel = model.pickerModel |> DatePicker.update subMsg }
                    , Cmd.none
                    )

        SetToday today ->
            ( { model | pickerModel = model.pickerModel |> DatePicker.setToday today }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



-- SETTINGS


settings : DatePicker.Settings
settings =
    let
        default =
            DatePicker.defaultSettings
    in
    { default
        | pickerAttributes = [ Border.width 1, Border.color (Element.rgb255 100 150 200) ]
        , headerAttributes = [ Element.width Element.fill, Font.bold, Background.color (Element.rgb255 80 180 255) ]
        , tableAttributes = [ padding 6, Background.color (Element.rgb255 200 230 255) ]
        , language = languagePt

        --     , weekdayAttributes = [ Font.color (Element.rgb255 50 50 50) ]
        --    , dayAttributes = []
        --    , wrongMonthDayAttributes = [ Font.light ]
        --    , selectedDayAttributes = [ Background.color (Element.rgb255 255 200 0) ]
        --    , previousMonthElement = Element.el [ Background.color (Element.rgb255 100 200 255), padding 12 ] <| Element.text "<"
        --    , nextMonthElement = Element.el [ Background.color (Element.rgb255 100 200 255), padding 12 ] <| Element.text ">"
    }



-- Helper functions for month names in Portuguese.


languagePt : Maybe DatePicker.Language
languagePt =
    Just
        { monthName = monthName True
        , weekdayName = weekdayName True
        , dayWithSuffix = \x -> String.fromInt x ++ "º"
        , monthNameShort = monthName False
        , weekdayNameShort = weekdayName False

        --         , weekdayName = weekdayName True
        --        , dayWithSuffix = \x -> String.fromInt x ++ "º"
        }


monthName : Bool -> Month -> String
monthName long month =
    let
        longName =
            case month of
                Jan ->
                    "Janeiro"

                Feb ->
                    "Fevereiro"

                Mar ->
                    "Março"

                Apr ->
                    "Abril"

                May ->
                    "Maio"

                Jun ->
                    "Junho"

                Jul ->
                    "Julho"

                Aug ->
                    "Agosto"

                Sep ->
                    "Setembro"

                Oct ->
                    "Outubro"

                Nov ->
                    "Novembro"

                Dec ->
                    "Dezembro"
    in
    if long then
        longName

    else
        String.slice 0 3 longName


weekdayName : Bool -> Weekday -> String
weekdayName long weekday =
    let
        longName =
            case weekday of
                Mon ->
                    "Segunda-feira"

                Tue ->
                    "Terça-feira"

                Wed ->
                    "Quarta-feira"

                Thu ->
                    "Quinta-feira"

                Fri ->
                    "Sexta-feira"

                Sat ->
                    "Sábado"

                Sun ->
                    "Domingo"
    in
    if long then
        longName

    else
        String.slice 0 3 longName



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
