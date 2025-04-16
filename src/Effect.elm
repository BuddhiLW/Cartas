port module Effect exposing
    ( Effect
    , none, batch
    , sendCmd, sendMsg
    , pushRoute, replaceRoute
    , pushRoutePath, replaceRoutePath
    , loadExternalUrl, back
    , map, toCmd
    , clearUser, downloadImage, downloadPdf, receiveImageUrl, receivePdfUrl, saveUser, signIn, signOut, storeFile
    )

{-|

@docs Effect

@docs none, batch
@docs sendCmd, sendMsg

@docs pushRoute, replaceRoute
@docs pushRoutePath, replaceRoutePath
@docs loadExternalUrl, back

@docs map, toCmd

-}

import Api.Role
import Browser.Navigation
import Dict exposing (Dict)
import File
import Json.Encode
import Route exposing (Route)
import Route.Path
import Shared.Model
import Shared.Msg
import Task
import Url exposing (Url)


type Effect msg
    = -- BASICS
      None
    | Batch (List (Effect msg))
    | SendCmd (Cmd msg)
      -- ROUTING
    | PushUrl String
    | ReplaceUrl String
    | LoadExternalUrl String
    | Back
      -- SHARED
    | SendSharedMsg Shared.Msg.Msg
      -- PORT
    | SendToLocalStorage { key : String, value : Json.Encode.Value }



-- SHARED


signIn :
    { token : String
    , id : String
    , name : String
    , image : String
    , email : String
    , role : String
    }
    -> Effect msg
signIn user =
    SendSharedMsg (Shared.Msg.SignIn user)


signOut : Effect msg
signOut =
    SendSharedMsg Shared.Msg.SignOut



-- BASICS


{-| Don't send any effect.
-}
none : Effect msg
none =
    None


{-| Send multiple effects at once.
-}
batch : List (Effect msg) -> Effect msg
batch =
    Batch


{-| Send a normal `Cmd msg` as an effect, something like `Http.get` or `Random.generate`.
-}
sendCmd : Cmd msg -> Effect msg
sendCmd =
    SendCmd


{-| Send a message as an effect. Useful when emitting events from UI components.
-}
sendMsg : msg -> Effect msg
sendMsg msg =
    Task.succeed msg
        |> Task.perform identity
        |> SendCmd



-- ROUTING


{-| Set the new route, and make the back button go back to the current route.
-}
pushRoute :
    { path : Route.Path.Path
    , query : Dict String String
    , hash : Maybe String
    }
    -> Effect msg
pushRoute route =
    PushUrl (Route.toString route)


{-| Same as `Effect.pushRoute`, but without `query` or `hash` support
-}
pushRoutePath : Route.Path.Path -> Effect msg
pushRoutePath path =
    PushUrl (Route.Path.toString path)


{-| Set the new route, but replace the previous one, so clicking the back
button **won't** go back to the previous route.
-}
replaceRoute :
    { path : Route.Path.Path
    , query : Dict String String
    , hash : Maybe String
    }
    -> Effect msg
replaceRoute route =
    ReplaceUrl (Route.toString route)


{-| Same as `Effect.replaceRoute`, but without `query` or `hash` support
-}
replaceRoutePath : Route.Path.Path -> Effect msg
replaceRoutePath path =
    ReplaceUrl (Route.Path.toString path)


{-| Redirect users to a new URL, somewhere external to your web application.
-}
loadExternalUrl : String -> Effect msg
loadExternalUrl =
    LoadExternalUrl


{-| Navigate back one page
-}
back : Effect msg
back =
    Back



-- INTERNALS


{-| Elm Land depends on this function to connect pages and layouts
together into the overall app.
-}
map : (msg1 -> msg2) -> Effect msg1 -> Effect msg2
map fn effect =
    case effect of
        None ->
            None

        Batch list ->
            Batch (List.map (map fn) list)

        SendCmd cmd ->
            SendCmd (Cmd.map fn cmd)

        PushUrl url ->
            PushUrl url

        ReplaceUrl url ->
            ReplaceUrl url

        Back ->
            Back

        LoadExternalUrl url ->
            LoadExternalUrl url

        SendSharedMsg sharedMsg ->
            SendSharedMsg sharedMsg

        SendToLocalStorage value ->
            SendToLocalStorage value


{-| Elm Land depends on this function to perform your effects.
-}
toCmd :
    { key : Browser.Navigation.Key
    , url : Url
    , shared : Shared.Model.Model
    , fromSharedMsg : Shared.Msg.Msg -> msg
    , batch : List msg -> msg
    , toCmd : msg -> Cmd msg
    }
    -> Effect msg
    -> Cmd msg
toCmd options effect =
    case effect of
        None ->
            Cmd.none

        Batch list ->
            Cmd.batch (List.map (toCmd options) list)

        SendCmd cmd ->
            cmd

        PushUrl url ->
            Browser.Navigation.pushUrl options.key url

        ReplaceUrl url ->
            Browser.Navigation.replaceUrl options.key url

        Back ->
            Browser.Navigation.back options.key 1

        LoadExternalUrl url ->
            Browser.Navigation.load url

        SendSharedMsg sharedMsg ->
            Task.succeed sharedMsg
                |> Task.perform options.fromSharedMsg

        SendToLocalStorage value ->
            sendToLocalStorage value



-- PORT: accessToken


port sendToLocalStorage :
    { key : String
    , value : Json.Encode.Value
    }
    -> Cmd msg


saveUser :
    { token : String
    , id : String
    , name : String
    , image : String
    , email : String
    , role : String
    }
    -> Effect msg
saveUser user =
    SendToLocalStorage
        { key = "user"
        , value =
            Json.Encode.object
                [ ( "token", Json.Encode.string user.token )
                , ( "id", Json.Encode.string user.id )
                , ( "name", Json.Encode.string user.name )
                , ( "image", Json.Encode.string user.image )
                , ( "email", Json.Encode.string user.email )
                , ( "role", Json.Encode.string user.role )
                ]
        }


clearUser : Effect msg
clearUser =
    SendToLocalStorage
        { key = "user"
        , value = Json.Encode.null
        }



-- PDF PORTS


port sendPdfRequest :
    { url : String
    , letterData : Json.Encode.Value
    , files :
        { photo : Maybe { name : String, mime : String } -- Just pass minimal file info
        , background : Maybe { name : String, mime : String }
        }
    }
    -> Cmd msg


port receivePdfUrl : (String -> msg) -> Sub msg


downloadPdf :
    { url : String
    , letterData : Json.Encode.Value
    , files : { photo : Maybe File.File, background : Maybe File.File }
    , onSuccess : String -> msg
    }
    -> Effect msg
downloadPdf config =
    SendCmd
        (sendPdfRequest
            { url = config.url
            , letterData = config.letterData
            , files =
                { photo =
                    Maybe.map
                        (\f ->
                            { name = File.name f
                            , mime = File.mime f
                            }
                        )
                        config.files.photo
                , background =
                    Maybe.map
                        (\f ->
                            { name = File.name f
                            , mime = File.mime f
                            }
                        )
                        config.files.background
                }
            }
        )


port receiveImageUrl : (String -> msg) -> Sub msg


port downloadImagePort : { url : String } -> Cmd msg


downloadImage :
    { url : String
    , onSuccess : String -> msg
    }
    -> Effect msg
downloadImage config =
    SendCmd (downloadImagePort { url = config.url })


port storeFile : { name : String, file : Json.Encode.Value } -> Cmd msg
