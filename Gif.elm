module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { topic : String
    , gifUrl : String
    , errorMsg : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "cats" "https://media1.giphy.com/media/gl8ymnpv4Sqha/giphy.gif" "", Cmd.none )



-- UPDATE


type Msg
    = MorePlease
    | NewGif (Result Http.Error String)
    | Topic String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( model, getRandomGif model.topic )

        Topic topic ->
            ( { model | topic = topic }, Cmd.none )

        NewGif (Ok newUrl) ->
            ( { model | gifUrl = newUrl, errorMsg = "" }, Cmd.none )

        NewGif (Err error) ->
            ( { model | errorMsg = toString error }, Cmd.none )


getRandomGif : String -> Cmd Msg
getRandomGif topic =
    let
        url =
            "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic

        request =
            Http.get url decodeGifUrl
    in
    Http.send NewGif request


decodeGifUrl : Decode.Decoder String
decodeGifUrl =
    Decode.at [ "data", "image_url" ] Decode.string



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Current Topic" ]
        , h2 [] [ text model.topic ]
        , img [ src model.gifUrl ] []
        , button [ onClick MorePlease ] [ text "More Please!" ]
        , select [ onItemChange Topic ] (List.map viewTopic allTopics)
        , div []
            [ h3 [] [ text model.errorMsg ]
            ]
        ]



-- https://robots.thoughtbot.com/building-custom-dom-event-handlers-in-elm


onItemChange : (String -> msg) -> Html.Attribute msg
onItemChange tagger =
    on "change" (Decode.map tagger Html.Events.targetValue)



-- this isn't being used
-- but it essentially gets made by doing onItemChange Topic


selectedTopicDecoder : Decode.Decoder Msg
selectedTopicDecoder =
    Decode.map Topic Html.Events.targetValue


viewTopic : String -> Html Msg
viewTopic topic =
    option [ value topic ] [ text topic ]


allTopics =
    [ "cats", "dogs", "goats", "squirrels" ]
