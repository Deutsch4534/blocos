module Session exposing (Msg(..), Session(..), User, UserData, subscriptions, update)

import Browser.Navigation as Nav
import Json.Decode as Decode
import Json.Encode as Encode
import Port.Blockstack as Blockstack
import Url.Builder


type Session
    = Anonymous
    | LoggedIn


type alias UserData =
    { username : String, name : Maybe String, profilePicture : Maybe String }


type alias User =
    ( Session, Maybe UserData )


type Msg
    = SignIn
    | SignOut
    | CheckAuthentication
    | RedirectHome
    | SessionChanged User


decodeUser : Encode.Value -> User
decodeUser value =
    let
        decodedUserData =
            Decode.decodeValue userDataDecoder value
    in
    case decodedUserData of
        Ok userData ->
            ( LoggedIn, Just userData )

        Err error ->
            -- ( Anonymous, Just { username = Decode.errorToString error } )
            ( Anonymous, Nothing )


userDataDecoder : Decode.Decoder UserData
userDataDecoder =
    Decode.map3 UserData
        (Decode.field "username" Decode.string)
        (Decode.field "name" (Decode.nullable Decode.string))
        (Decode.field "profilePicture" (Decode.nullable Decode.string))


subscriptions : User -> Sub Msg
subscriptions user =
    Blockstack.authenticated (\value -> SessionChanged (decodeUser value))


redirectHome : Session -> Nav.Key -> Cmd Msg
redirectHome session navKey =
    case session of
        LoggedIn ->
            Nav.pushUrl navKey (Url.Builder.absolute [ "dashboard" ] [])

        Anonymous ->
            Nav.pushUrl navKey (Url.Builder.absolute [ "" ] [])


update : Msg -> ( User, Nav.Key ) -> ( User, Cmd Msg )
update msg ( user, navKey ) =
    case msg of
        SignIn ->
            ( user, Blockstack.authenticate () )

        SignOut ->
            ( user, Blockstack.signOut () )

        CheckAuthentication ->
            ( user, Blockstack.checkAuthentication () )

        SessionChanged ( session, userData ) ->
            let
                newUser =
                    ( session, userData )

                redirect =
                    redirectHome session navKey
            in
            ( newUser, redirect )

        RedirectHome ->
            let
                ( session, userData ) =
                    user
            in
            ( user, redirectHome session navKey )
