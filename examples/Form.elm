module Form exposing (..)

import Field exposing (..)
import Field.ValidationResult exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Regex exposing (Regex)


type FormMessage
    = Error (List String)
    | Pass String
    | None


type alias Model =
    { name : Field String String
    , email : Field String String
    , phone : Field String String
    , message : FormMessage
    }


init =
    { name = Field (validateEmpty "Name") ""
    , email = Field (validateEmpty "Email" >> andThen validateEmail) ""
    , phone = Field validateNumbersOnly ""
    , message = None
    }


type Msg
    = OnNameInput String
    | OnEmailInput String
    | OnPhoneInput String
    | OnSubmit


validateEmpty : String -> String -> ValidationResult String String
validateEmpty n s =
    case s of
        "" ->
            Failed <| n ++ " cannot be empty"

        _ ->
            Passed s


validEmail : Regex
validEmail =
    Regex.regex "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
        |> Regex.caseInsensitive


validNumbersOnly : Regex
validNumbersOnly =
    Regex.regex "^\\d{10}$" |> Regex.caseInsensitive


validateEmail : String -> ValidationResult String String
validateEmail s =
    case (Regex.contains validEmail s) of
        True ->
            Passed s

        False ->
            Failed "Please enter a valid email"


validateNumbersOnly : String -> ValidationResult String String
validateNumbersOnly s =
    case (Regex.contains validNumbersOnly s) of
        True ->
            Passed s

        False ->
            Failed "Please enter a valid phone number"


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnNameInput name ->
            { model | name = Field.update model.name name }

        OnEmailInput email ->
            { model | email = Field.update model.email email }

        OnPhoneInput phone ->
            { model | phone = Field.update model.phone phone }

        OnSubmit ->
            let
                fields =
                    [ model.name, model.email, model.phone ]

                errors =
                    validateAll fields
                        |> List.filter
                            (\err ->
                                case err of
                                    Failed err ->
                                        True

                                    Passed _ ->
                                        False
                            )

                isValid =
                    isAllValid fields

                message =
                    case isValid of
                        True ->
                            Pass "Good to Go"

                        False ->
                            Error <|
                                List.map
                                    (\err ->
                                        case err of
                                            Failed err ->
                                                err

                                            _ ->
                                                "Unknown error"
                                    )
                                    errors
            in
                { model | message = message }


view : Model -> Html Msg
view model =
    let
        displayMessage =
            case model.message of
                Pass m ->
                    m

                Error m ->
                    String.join ", " m

                None ->
                    ""
    in
        div [ class "form" ]
            [ div [] [ text displayMessage ]
            , text "Name:"
            , input [ id "name", type_ "text", onInput OnNameInput ] []
            , text "Email:"
            , input [ id "email", type_ "text", onInput OnEmailInput ] []
            , text "Phone:"
            , input [ id "phone", type_ "text", onInput OnPhoneInput ] []
            , button [ id "submit", onClick OnSubmit ] [ text "Validate Form" ]
            ]


main : Program Never Model Msg
main =
    beginnerProgram
        { model = init
        , view = view
        , update = update
        }
