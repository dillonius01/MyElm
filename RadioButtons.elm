module Main exposing (..)

import Html exposing (Attribute, Html, div, fieldset, input, label, section, text)
import Html.Attributes exposing (name, style, type_)
import Html.Events exposing (onClick)
import Markdown


main =
    Html.beginnerProgram { model = chapter1, update = update, view = view }



-- MODEL


type alias Model =
    { fontSize : FontSize
    , content : String
    }


chapter1 : Model
chapter1 =
    Model Medium intro


intro : String
intro =
    """

# Moby Dick

## Chapter 1

Call me Ishmael. Some years ago- never mind how long precisely- having little or no money in my
purse, and nothing particular to interest me on shore, I thought I would sail about a little and
see the watery part of the world. It is a way I have of driving off the spleen and regulating the
 circulation. Whenever I find myself growing grim about the mouth; whenever it is a damp, drizzly
 November in my soul; whenever I find myself involuntarily pausing before coffin warehouses,
 and bringing up the rear of every funeral I meet; and especially whenever my hypos get such an
 upper hand of me, that it requires a strong moral principle to prevent me from deliberately stepping
 into the street, and methodically knocking people's hats off- then, I account it high time to get
 to sea as soon as I can.


"""


type FontSize
    = Small
    | Medium
    | Large



-- UPDATE


type Msg
    = SwitchTo FontSize


update : Msg -> Model -> Model
update msg model =
    case msg of
        SwitchTo newFontSize ->
            { model | fontSize = newFontSize }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewPicker
            [ ( "Small", SwitchTo Small )
            , ( "Medium", SwitchTo Medium )
            , ( "Large", SwitchTo Large )
            ]
        , Markdown.toHtml [ sizeToStyle model.fontSize ] model.content
        ]


sizeToStyle : FontSize -> Attribute msg
sizeToStyle fontSize =
    let
        size =
            case fontSize of
                Small ->
                    "0.8em"

                Medium ->
                    "1em"

                Large ->
                    "1.2em"
    in
    style [ ( "font-size", size ) ]


viewPicker : List ( String, msg ) -> Html msg
viewPicker options =
    fieldset [] (List.map radio options)


radio : ( String, msg ) -> Html msg
radio ( value, msg ) =
    label []
        [ input [ type_ "radio", name "font-size", onClick msg ] []
        , text value
        ]
