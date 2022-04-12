module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, map4, field, int, string)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

type Model
    = Failure
    | Loading
    | Success Quote

type alias Quote =
    { quote : String
    , source : String
    , author : String
    , year : Int
    }

init : a -> (Model, Cmd Msg)
init _ =
    (Loading, getRandomQuote)


type Msg
    = MorePlease
    | GotQuote (Result Http.Error Quote) --Return Result which could be either Http.Error or Quote



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MorePlease ->
            (Loading, getRandomQuote)

        GotQuote result ->
            case result of
                Ok quote ->
                    (Success quote, Cmd.none)
                Err _ ->
                    (Failure, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
    [ h2 [] [text " Random Quotes"]
    , viewQuote model
    ]


viewQuote : Model -> Html Msg
viewQuote model =
  case model of
    Failure ->
      div []
        [ text "I could not load a random quote for some reason. "
        , button [ onClick MorePlease ] [ text "Try Again!" ]
        ]

    Loading ->
      text "Loading..."

    Success quote ->
      div []
        [ button [ onClick MorePlease, style "display" "block" ] [ text "More Please!" ]
        , blockquote [] [ text quote.quote ]
        , p [ style "text-align" "right" ]
            [ text "— "
            , cite [] [ text quote.source ]
            , text (" by " ++ quote.author ++ " (" ++ String.fromInt quote.year ++ ")")
            ]
        ]


getRandomQuote =
    Http.get
        { url = "https://elm-lang.org/api/random-quotes"
         , expect = Http.expectJson GotQuote quoteDecoder}


ageDecoder : Decoder Int
ageDecoder =
    field "age" int

nameDecoder : Decoder String
nameDecoder =
    field "name" string



quoteDecoder : Decoder Quote
quoteDecoder =
    map4 Quote
        (field "quote" string)
        (field "source" string)
        (field "author" string)
        (field "year" int)