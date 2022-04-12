module HttpExamples exposing (..)

import Browser
import Html exposing (Html, button, div, h3, li, text, ul)
import Html.Events exposing (onClick)
import Http
import String exposing (..)


type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int
    | BadBody String


type alias  Model =
    { nicknames: List String
    , errorMessage: Maybe String
    }

view: Model -> Html Msg
view model =
    div []
    [ button [onClick SendHttpRequest] [text "Get data from the server"]
    , viewNicknamesOrError model
    ]

viewNicknamesOrError : Model -> Html Msg
viewNicknamesOrError model =
    case model.errorMessage of
        Just message ->
            viewError message
        Nothing ->
            viewNicknames model.nicknames

viewError errorMessage =
    let
        errorHeading = "Unable to fetch data from the url"
    in
       div []
        [ h3 [] [text errorHeading]
        , text ("Error: " ++ errorMessage)
        ]

viewNicknames : List String -> Html Msg
viewNicknames nicknames =
       div []
        [ h3 [] [text "Old School Characters"]
        , ul [] (List.map viewNickname nicknames)
        ]

viewNickname : String -> Html Msg
viewNickname nickname =
    li [] [text nickname]



type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error String)

url =
    "http://localhost:5016/old-sccchool.txt"

getNicknames : Cmd Msg
getNicknames =
    Http.get
        { url = url
        , expect = Http.expectString DataReceived
        }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SendHttpRequest ->
            (model, getNicknames)

        DataReceived (Ok nicknamesStr) ->
            let
                nicknames =
                    String.split "," nicknamesStr
            in
            ({model | nicknames = nicknames}, Cmd.none)
        DataReceived (Err httpError) ->
            ( { model
                | errorMessage = Just (buildErrorMessage httpError)
            }
            , Cmd.none)


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server"

        Http.BadStatus statusCode ->
            "Request failed with the code " ++ fromInt statusCode

        Http.BadBody message ->
            message




init _ =
    ( { nicknames = []
      , errorMessage = Nothing
      }
     , Cmd.none
     )

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
