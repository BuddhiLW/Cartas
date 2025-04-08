module Pages.SignInTmp exposing (Model, Msg, page)

import Api.Me
import Api.SignIn
import Colors.Opaque as Color
import Effect exposing (Effect)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Http
import Page exposing (Page)
import Route exposing (Route)
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
    { email : String
    , password : String
    , isSubmittingForm : Bool
    , errors : List Api.SignIn.Error
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { email = ""
      , password = ""
      , isSubmittingForm = False
      , errors = []
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = UserUpdatedInput Field String
    | UserSubmittedForm
    | SignInApiResponded (Result (List Api.SignIn.Error) Api.SignIn.Data)
    | MeApiResponded String (Result Http.Error Api.Me.User)


type Field
    = Email
    | Password


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        UserUpdatedInput Email value ->
            ( { model | email = value }
            , Effect.none
            )

        UserUpdatedInput Password value ->
            ( { model | password = value }
            , Effect.none
            )

        UserSubmittedForm ->
            ( { model | isSubmittingForm = True, errors = [] }
            , Api.SignIn.post
                { onResponse = SignInApiResponded
                , email = model.email
                , password = model.password
                }
            )

        SignInApiResponded (Ok { token }) ->
            ( model
            , Api.Me.get
                { token = token
                , onResponse = MeApiResponded token
                }
            )

        SignInApiResponded (Err errors) ->
            ( { model | isSubmittingForm = False, errors = errors }
            , Effect.none
            )

        MeApiResponded token (Ok user) ->
            ( { model | isSubmittingForm = False }
            , Effect.signIn
                { id = user.id
                , name = user.name
                , image = user.image
                , email = user.email
                , role = user.role
                , token = token
                }
            )

        MeApiResponded _ (Err httpError) ->
            let
                error : Api.SignIn.Error
                error =
                    { field = Nothing
                    , message =
                        case httpError of
                            Http.BadStatus 401 ->
                                "Solicitação inválida. Verifique suas informações."

                            Http.BadStatus 404 ->
                                "Usuário não encontrado. Verifique suas credenciais."

                            Http.BadStatus 500 ->
                                "Erro no servidor. Tente novamente mais tarde."

                            _ ->
                                "Ocorreu um erro inesperado."
                    }
            in
            ( { model
                | isSubmittingForm = False
                , errors = [ error ]
              }
            , Effect.signOut
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Entrar"
    , attributes = []
    , element =
        Element.html (viewPage model)
    }


viewPage : Model -> Html Msg
viewPage model =
    Element.layout
        [ Element.centerY
        , Element.centerX
        ]
        (Element.column [ Element.centerX, Element.centerY ]
            [ Element.el [ Font.bold, Font.size 50, Element.paddingXY 0 20 ] (Element.text "Notas de Falecimento")
            , Element.el [ Element.centerX ] (viewForm model)
            ]
        )


viewForm : Model -> Element.Element Msg
viewForm model =
    --   Html.form [ Attr.class "box", Html.Events.onSubmit UserSubmittedForm ]
    Element.column
        [ Element.width Element.fill
        , Element.padding 20
        , Element.spacing 20
        , Border.width 1
        , Border.shadow { offset = ( 3.0, 2.0 ), size = 0.4, blur = 6, color = Color.gray }
        , Border.color Color.gray
        , Border.rounded 10
        ]
        [ viewFormInput
            { field = Email
            , value = model.email
            , error = findFieldError "email" model
            }
        , viewFormInput
            { field = Password
            , value = model.password
            , error = findFieldError "password" model
            }
        , viewFormControls model
            |> Element.html
        ]


viewFormEmail :
    { field : Field
    , value : String
    , error : Maybe Api.SignIn.Error
    }
    -> Element.Element Msg
viewFormEmail options =
    Element.column
        [ Element.paddingXY 0 0
        , Element.spacing 5
        ]
        [ Input.email
            [-- Element.paddingEach { left = 0, right = 0, bottom = 10, top = 0 }
             -- , Element.spacingXY 0 0
            ]
            { label =
                Input.labelAbove
                    [ Font.size 30
                    , Font.bold
                    , Element.paddingXY 0 5
                    ]
                    (Element.text (fromFieldToLabel options.field))
            , text = options.value
            , placeholder =
                Just
                    (Input.placeholder []
                        (Element.text (fromFieldToLabel options.field))
                    )
            , onChange = UserUpdatedInput options.field
            }
        , case options.error of
            Just error ->
                Element.el
                    [ Font.size 12
                    , Font.color Color.red
                    , Element.paddingXY 0 5
                    ]
                    (Element.text error.message)

            Nothing ->
                Element.text ""
        ]


viewFormInput :
    { field : Field
    , value : String
    , error : Maybe Api.SignIn.Error
    }
    -> Element.Element Msg
viewFormInput options =
    case options.field of
        Email ->
            viewFormEmail options

        Password ->
            viewFormPassword options


viewFormPassword :
    { field : Field
    , value : String
    , error : Maybe Api.SignIn.Error
    }
    -> Element.Element Msg
viewFormPassword options =
    Element.column
        [ Element.paddingXY 0 0
        , Element.spacing 5
        ]
        [ Input.currentPassword
            [ Font.size 30 ]
            { label =
                Input.labelAbove
                    [ Font.size 40
                    , Font.bold
                    , Element.paddingXY 0 5
                    ]
                    (Element.text (fromFieldToLabel options.field))
            , text = options.value
            , placeholder =
                Just
                    (Input.placeholder []
                        (Element.text (fromFieldToLabel options.field))
                    )
            , onChange = UserUpdatedInput options.field
            , show = False
            }
        , case options.error of
            Just error ->
                Element.el
                    [ Font.size 12
                    , Font.color Color.red
                    , Element.paddingXY 0 5
                    ]
                    (Element.text error.message)

            Nothing ->
                Element.text ""
        ]



-- [ Html.label [ Attr.class "label" ] [ Html.text (fromFieldToLabel options.field) ]
-- , Html.div [ Attr.class "control" ]
--     [ Html.input
--         [ Attr.class "input"
--         , Attr.classList
--             [ ( "is-danger", options.error /= Nothing )
--             ]
--         , Attr.type_ (fromFieldToInputType options.field)
--         , Attr.value options.value
--         , Html.Events.onInput (UserUpdatedInput options.field)
--         ]
--         []
--     ]
-- , case options.error of
--     Just error ->
--         Html.p
--             [ Attr.class "help is-danger" ]
--             [ Html.text error.message ]
--     Nothing ->
--         Html.text ""
-- ]


fromFieldToLabel : Field -> String
fromFieldToLabel field =
    case field of
        Email ->
            "Email"

        Password ->
            "Senha"


fromFieldToInputType : Field -> String
fromFieldToInputType field =
    case field of
        Email ->
            "email"

        Password ->
            "password"


viewFormControls : Model -> Html Msg
viewFormControls model =
    Html.div []
        [ Html.div [ Attr.class "field is-grouped is-grouped-right" ]
            [ Html.div
                [ Attr.class "control" ]
                [ Html.button
                    [ Attr.class "button is-link"
                    , Attr.disabled model.isSubmittingForm
                    , Attr.classList [ ( "is-loading", model.isSubmittingForm ) ]
                    ]
                    [ Html.text "Entrar" ]
                ]
            ]
        , case findFormError model of
            Just error ->
                Html.p
                    [ Attr.class "help content is-danger" ]
                    [ Html.text error.message ]

            Nothing ->
                Html.text ""
        ]



-- ERRORS


findFieldError : String -> Model -> Maybe Api.SignIn.Error
findFieldError field model =
    let
        hasMatchingField : Api.SignIn.Error -> Bool
        hasMatchingField error =
            error.field == Just field
    in
    model.errors
        |> List.filter hasMatchingField
        |> List.head


findFormError : Model -> Maybe Api.SignIn.Error
findFormError model =
    let
        doesntHaveField : Api.SignIn.Error -> Bool
        doesntHaveField error =
            error.field == Nothing
    in
    model.errors
        |> List.filter doesntHaveField
        |> List.head
