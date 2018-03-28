module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { time : Time
    , isPaused : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model 0 False, Cmd.none )



-- UPDATE


type Msg
    = Tick Time
    | Pause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

        Pause ->
            ( { model | isPaused = not model.isPaused }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if not model.isPaused then
        Time.every second Tick
    else
        Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        angle =
            turns (Time.inMinutes model.time)

        handX =
            toString (50 + 40 * cos angle)

        handY =
            toString (50 + 40 * sin angle)
    in
    div []
        [ svg [ viewBox "0 0 100 100", width "300px" ]
            [ circle [ cx "50", cy "50", r "45", fill "#0b79ce" ] []
            , line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] []
            ]
        , button [ onClick Pause ] [ Html.text <| mapBtnLabel model ]
        ]


mapBtnLabel : Model -> String
mapBtnLabel model =
    if model.isPaused then
        "Resume"
    else
        "Pause"
