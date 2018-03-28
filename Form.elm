module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Regex exposing (regex)


main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , age : String
    , submitted : Bool
    }


model : Model
model =
    Model "" "" "" "" False


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String
    | Submitted


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }

        Age age ->
            { model | age = age }

        Submitted ->
            { model | submitted = True }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Name", onInput Name ] []
        , input [ type_ "password", placeholder "Password", onInput Password ] []
        , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
        , input [ type_ "text", placeholder "Age", onInput Age ] []
        , button [ onClick Submitted ] [ text "Submit" ]
        , model |> validatePassword |> mapStyle
        ]


type ValidationStatus
    = Valid
    | PasswordTooShort
    | PasswordMismatch
    | PasswordCharactersLame
    | AgeNotNumber
    | SkipValidation


mapStyle : ValidationStatus -> Html validationMsg
mapStyle status =
    let
        ( color, message ) =
            case status of
                Valid ->
                    ( "green", "OK" )

                PasswordTooShort ->
                    ( "red", "Password must be at least 8 chars long" )

                PasswordMismatch ->
                    ( "red", "Passwords do not match" )

                PasswordCharactersLame ->
                    ( "red", "Password must contain lowercase, uppercase, and special characters" )

                AgeNotNumber ->
                    ( "red", "Your age must be a positive integer" )

                SkipValidation ->
                    ( "black", "" )
    in
    div [ style [ ( "color", color ) ] ] [ text message ]


validatePassword : Model -> ValidationStatus
validatePassword model =
    if not model.submitted then
        SkipValidation
    else if String.length model.password <= 8 then
        PasswordTooShort
    else if model.password /= model.passwordAgain then
        PasswordMismatch
    else if not (Regex.contains (regex "^[1-9]+[0-9]*$") model.age) then
        AgeNotNumber
    else if
        not (Regex.contains (regex "[a-z]") model.password)
            || not (Regex.contains (regex "[A-Z]") model.password)
            || not (Regex.contains (regex "[0-9]") model.password)
            || not (Regex.contains (regex "[!@#$%^&*()-+=]") model.password)
    then
        PasswordCharactersLame
    else
        Valid
