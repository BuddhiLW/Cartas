module Pages.Letters exposing (Model, Msg, page)

import Api.Letter
import Auth
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
    in
    ( { letterForm = emptyLetterForm
      , isLoading = False
      , errors = []
      , photoUploader = photoUploaderModel
      , backgroundUploader = backgroundUploaderModel
      }
    , Effect.batch
        [ Effect.map PhotoUploaderMsg (Effect.sendCmd photoUploaderCmd)
        , Effect.map BackgroundUploaderMsg (Effect.sendCmd backgroundUploaderCmd)
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | PhotoUploaderMsg ImageUpload.Msg
    | BackgroundUploaderMsg ImageUpload.Msg


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.Letters"
    , attributes = []
    , element =
        Html.form [ Html.Events.onSubmit NoOp ]
            [ Html.h2 [] [ Html.text "Letter Information Form" ]
            , Html.div []
                [ Html.label [] [ Html.text "Name:" ]
                , Html.input [ Attr.type_ "text", Attr.placeholder "Enter letter name" ] []
                ]
            , Html.div []
                [ Html.label [] [ Html.text "Year of Birth:" ]
                , Html.input [ Attr.type_ "number", Attr.placeholder "Year of Birth" ] []
                ]
            , Html.div []
                [ Html.label [] [ Html.text "Year of Death:" ]
                , Html.input [ Attr.type_ "number", Attr.placeholder "Year of Death" ] []
                ]
            , Html.div []
                [ Html.label [] [ Html.text "Event Date:" ]
                , Html.input [ Attr.type_ "date" ] []
                ]
            , Html.div []
                [ Html.label [] [ Html.text "Graveyard Name:" ]
                , Html.input [ Attr.type_ "text", Attr.placeholder "Graveyard Name" ] []
                ]
            , Html.h3 [] [ Html.text "Photo Upload" ]
            , ImageUpload.view model.photoUploader |> Html.map PhotoUploaderMsg
            , Html.h3 [] [ Html.text "Background Upload" ]
            , ImageUpload.view model.backgroundUploader |> Html.map BackgroundUploaderMsg
            , Html.button [ Attr.type_ "submit" ] [ Html.text "Submit" ]
            ]
            |> Element.html
    }
