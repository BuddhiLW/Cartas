module Auth exposing (User, onPageLoad, viewCustomPage)

import Auth.Action
import Dict
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model
import View exposing (View)


type alias User =
    Shared.Model.User


{-| Called before an auth-only page is loaded.
-}
onPageLoad : Shared.Model -> Route () -> Auth.Action.Action User
onPageLoad shared _ =
    case shared.user of
        Just user ->
            Auth.Action.loadPageWithUser user

        Nothing ->
            Auth.Action.PushRoute
                { path = Route.Path.SignIn
                , query = Dict.empty
                , hash = Nothing
                }


{-| Renders whenever `Auth.Action.loadCustomPage` is returned from `onPageLoad`.
-}
viewCustomPage : Shared.Model -> Route () -> View Never
viewCustomPage shared route =
    View.fromString "Loading..."
