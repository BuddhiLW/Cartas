module Pages.NotFound_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (Element, el, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
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
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { attributes = []
    , title = "404"
    , element =
        Element.el
            [ Element.padding 20
            , Element.centerX
            , Element.centerY
            , Element.spacing 20
            , Background.color (Element.rgb 255 255 255)
            , Border.rounded 8
            , Border.width 1
            , Border.color (Element.rgb 0 0 0)
            , Font.size 36
            ]
            (Element.column [ Element.centerX, Element.spacing 20 ]
                [ Element.text "Hum..."
                , Element.text "Essa página não foi encontrada..."
                , Element.el
                    [ Element.paddingXY 0 20
                    , Element.centerX
                    ]
                    (Element.link
                        [ Element.padding 10
                        , Background.color (Element.rgb 0 0 255)
                        , Border.rounded 4
                        , Font.color (Element.rgb 255 255 255)
                        , Font.size 36
                        , Element.centerX
                        , Element.paddingXY 5 20
                        , Element.mouseOver
                            [ Background.color (Element.rgb 0 0 100)
                            , Font.color (Element.rgb 200 200 255)
                            , Border.color (Element.rgb 0 0 210)
                            ]
                        ]
                        { label = Element.text "Voltar à página inicial!"
                        , url = "/sign-in"
                        }
                    )
                ]
            )
    }
