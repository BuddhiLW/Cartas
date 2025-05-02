module Pages.SignIn exposing (Model, Msg, page, subscriptions, update, view)

import Api.Me
import Api.SignIn
import Effect exposing (Effect)
import Element exposing (..)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Http
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init { shared = shared }
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
    , shared : Shared.Model
    }


init : { shared : Shared.Model } -> () -> ( Model, Effect Msg )
init { shared } () =
    ( { email = "funerariafrancana@yahoo.com.br"
      , password = ""
      , isSubmittingForm = False
      , errors = []
      , shared = shared
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
                , baseUrl = model.shared.baseUrl
                }
            )

        SignInApiResponded (Ok { token }) ->
            -- ( { model | isSubmittingForm = False }
            -- , Effect.signIn { token = token }
            -- )
            ( model
            , Api.Me.get
                { token = token
                , onResponse = MeApiResponded token
                , baseUrl = model.shared.baseUrl
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
                                "Solicitação inválida. Por favor, verifique suas informações."

                            Http.BadStatus 404 ->
                                "Usuário não encontrado. Por favor, verifique suas credenciais."

                            Http.BadStatus 500 ->
                                "Erro no servidor. Por favor, tente novamente mais tarde."

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
    Html.div
        [ Attr.class "is-flex is-justify-content-center is-align-items-center "
        , Attr.style "min-height" "100vh"
        , Attr.style "background" "linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%)"
        ]
        [ Html.div [ Attr.class "container" ]
            [ Html.div
                [ Attr.class "box mt-2 "
                , Attr.style "max-width" "800px"
                , Attr.style "margin" "0 auto"
                , Attr.style "box-shadow" "0 4px 10px rgba(0, 0, 0, 0.1)"
                ]
                [ Html.div [ Attr.class "has-background-primary has-text-white p-4 " ]
                    [ Html.div [ Attr.class "is-flex is-align-items-center is-justify-content-center" ]
                        [ Html.img [ Attr.src "/static/logo.png", Attr.style "width" "100px" ] []
                        , Html.h1 [ Attr.class "title is-3 has-text-white" ] [ Html.text "Notas de Falecimento" ]
                        ]
                    , Html.p [ Attr.class "subtitle is-6 has-text-white has-text-centered" ] [ Html.text "Por favor, faça login para continuar" ]
                    ]
                , Html.div [ Attr.class "p-4" ]
                    [ Html.form [ Attr.id "loginForm", Html.Events.onSubmit UserSubmittedForm ]
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

                        -- , Html.div [ Attr.class "has-text-centered mt-4" ]
                        --     [ Html.a [ Attr.href "#", Attr.class "has-text-grey" ] [ Html.text "Esqueceu a senha?" ] ]
                        ]
                    ]
                ]
            ]
        ]


viewFormInput :
    { field : Field
    , value : String
    , error : Maybe Api.SignIn.Error
    }
    -> Html Msg
viewFormInput options =
    Html.div [ Attr.class "field mb-5" ]
        [ Html.label [ Attr.class "label" ] [ Html.text (fromFieldToLabel options.field) ]
        , Html.div [ Attr.class "control has-icons-left" ]
            [ Html.input
                [ Attr.class "input"
                , Attr.type_ (fromFieldToInputType options.field)
                , Attr.placeholder
                    (if options.field == Email then
                        "Digite seu e-mail"

                     else
                        "Digite sua senha"
                    )
                , Attr.value options.value
                , Html.Events.onInput (UserUpdatedInput options.field)
                ]
                []
            , Html.span [ Attr.class "icon is-small is-left" ]
                [ Html.i
                    [ Attr.class
                        (if options.field == Email then
                            "fas fa-envelope"

                         else
                            "fas fa-lock"
                        )
                    ]
                    []
                ]
            ]
        , case options.error of
            Just error ->
                Html.p [ Attr.class "help is-danger" ] [ Html.text error.message ]

            Nothing ->
                Html.text ""
        ]


fromFieldToLabel : Field -> String
fromFieldToLabel field =
    case field of
        Email ->
            "Endereço de e-mail"

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
        [ Html.div [ Attr.class "field" ]
            [ Html.div [ Attr.class "control" ]
                [ Html.button
                    [ Attr.type_ "submit"
                    , Attr.class "button is-primary is-fullwidth"
                    , Attr.disabled model.isSubmittingForm
                    , Attr.classList
                        [ ( "is-loading", model.isSubmittingForm ) ]
                    ]
                    [ Html.span [ Attr.class "icon" ]
                        [ Html.i [ Attr.class "fas fa-sign-in-alt" ] [] ]
                    , Html.span [] [ Html.text "Entrar" ]
                    ]
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
