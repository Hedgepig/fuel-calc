module Main exposing (..)

import Browser
import Html exposing (Html, text, div, span, input)
import Html.Attributes exposing (src, placeholder, value, type_, step, class)
import Html.Events exposing (onInput)
import Http
import Json.Decode exposing (Decoder, field, string)
import Debug exposing (log)

import MattValidate as Validate exposing (Validator, ifBlank, ifNotFloat, ifNotInt, validate)


--https://api.openrouteservice.org/geocode/search?api_key=5b3ce3597851110001cf6248ef46698a7f8c4f49b0fe3a4dfcdb21d8&text=tn26%203bb

---- MODEL ----

api_key = "5b3ce3597851110001cf6248ef46698a7f8c4f49b0fe3a4dfcdb21d8"
api_url = "https://api.openrouteservice.org/"

{-
makeRequest : String -> String -> Cmd msg
makeRequest fn params =
    let
        url = api_url++fn++"?api_key="++api_key++"&"++params
    in
        Http.get
            { url = url
            , expect = Http.expectJson
            }

 -}

type alias Price = Int

type alias Calculated = Int

type alias InputItem = { placeholder : String
                       , title : String
                       , field : Field
                       , accessor : Model -> Maybe String
                       }

type alias ParsedInput = { fuelConsumption : Float
                         , fuelPrice : Float
                         , distance : Float
                         }

type alias ValidationError = ( Field, String )

type alias IsInputEdited = Bool

displayPrice : String -> Price -> String
displayPrice symbol price =
    let
        hundreds = String.fromInt (price // 100)
        tens = String.fromInt (modBy 100 price)
    in
        symbol ++ hundreds ++ "." ++ (String.padRight 2 '0' tens)

displayGbp : Price -> String
displayGbp =
    displayPrice "£"

type alias Model =
    {
  --, addressFromInput : String
  --, addressToInput : String
      fuelPriceInput : Maybe String
    , distanceInput : Maybe String
    , fuelConsumptionInput : Maybe String
    , errors : List ValidationError
    , result : Maybe Price
    }

init : ( Model, Cmd Msg )
init =
    ( { fuelPriceInput = Nothing
      , distanceInput = Nothing
      , fuelConsumptionInput = Nothing
      , errors = []
      , result = Nothing
    }, Cmd.none )

toFloatDefault0 : Maybe String -> Float
toFloatDefault0 number =
    number
        |> Maybe.withDefault ""
        |> String.toFloat
        |> Maybe.withDefault 0

calculateCost : Model -> Price
calculateCost model =
    let
        -- These numbers are designed to normalise the different units
        -- If needed they can be refactored into the model so users from
        -- different countries can easily convert into their number
        distanceModifier = 1.609 -- Mile to Km
        fuelConsumptionModifier = 0.354 -- MPG to Km per Litre
        fuelPriceModifier = 1 -- Pence per litre is correct
        distance = (toFloatDefault0 model.distanceInput) * distanceModifier
        fuelPrice = (toFloatDefault0 model.fuelPriceInput) * fuelPriceModifier
        fuelConsumption = (toFloatDefault0 model.fuelConsumptionInput) * fuelConsumptionModifier
    in
        round (fuelPrice * (distance / fuelConsumption))

access : (Model -> Maybe String) -> Model -> String
access accessor model =
    Maybe.withDefault "" (accessor model)

modelValidator : Validator ( Field, String ) Model
modelValidator =
    Validate.all
        [ ifNotFloat (access .fuelConsumptionInput) ( FuelConsumptionInput, "MPG must be a valid number" )
        , ifBlank (access .fuelConsumptionInput) ( FuelConsumptionInput, "" )
        , ifNotFloat (access .distanceInput) ( DistanceInput, "Distance must be a valid number")
        , ifBlank (access .distanceInput) ( DistanceInput, "" )
        , ifNotFloat (access .fuelPriceInput) ( FuelPriceInput, "Pence per litre must be a valid number")
        , ifBlank (access .fuelPriceInput) ( FuelPriceInput, "")
        ]


---- UPDATE ----


type Field = DistanceInput
           | FuelConsumptionInput
           | FuelPriceInput

type Msg
    = NoOp
    | SetField Field String

setField : Field -> String -> Model -> Model
setField field value model =
    case field of
        FuelConsumptionInput ->
            { model | fuelConsumptionInput = Just value }
        FuelPriceInput ->
            { model | fuelPriceInput = Just value }
        DistanceInput ->
            { model | distanceInput = Just value }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        SetField field value ->
            ( model
            |> setField field value
            |> setErrors
            |> setResult
            , Cmd.none
            )

setErrors : Model -> Model
setErrors model =
    case (validate modelValidator model) of
        Ok result ->
            { model | errors = [] }
        Err errors ->
            { model | errors = errors }

setResult : Model -> Model
setResult model =
    if List.length model.errors == 0 then
       { model | result = Just (calculateCost model) }
    else
        model



---- VIEW ----

view : Model -> Html Msg
view model =
    div [ class "container" ] [
         div [ class "inputs_container" ]
             [ numberView fuelConsumptionItem model
             , numberView fuelPriceItem model
             , numberView distanceItem model
             ]
        , showResult model
        ]

numberView : InputItem -> (Model -> Html Msg)
numberView item model =
    let
        val = item.accessor model
    in
        div [ class "number_input_container" ]
            [ div [ class "number_input_title" ]
                  [ text item.title ]
            , div [ class "number_input" ]
                  [ input [ placeholder item.placeholder
                          , type_ "number"
                          , step "0.01"
                          , value (Maybe.withDefault "" val)
                          , onInput (SetField item.field)
                          ] []
                  ]
            ,
                case val of
                    Nothing ->
                        text ""
                    Just v ->
                        showErrors model item.field
            ]

showErrors : Model -> Field -> Html Msg
showErrors model f =
    span [] [ text (model.errors
                  |> List.filter (\err -> Tuple.first err == f)
                  |> List.map Tuple.second
                  |> List.head
                  |> Maybe.withDefault ""
                  )
        ]

showResult : Model -> Html Msg
showResult  model =
    div [ class "result_container" ]
        [ div [ class "result_title" ] [ text "Trip Cost" ]
        , case model.result of
              Just result ->
                  div [ class "result" ] [ text (displayGbp result) ]
              Nothing ->
                  text ""
        ]

distanceItem : InputItem
distanceItem = { placeholder = "Enter distance (miles)"
               , title = "Distance (miles)"
               , field = DistanceInput
               , accessor = .distanceInput
               }

fuelConsumptionItem : InputItem
fuelConsumptionItem = { placeholder = "Enter MPG"
                      , title = "MPG"
                      , field = FuelConsumptionInput
                      , accessor = .fuelConsumptionInput
                      }

fuelPriceItem : InputItem
fuelPriceItem = { placeholder = "Enter pence per litre"
                , title = "Pence per litre"
                , field = FuelPriceInput
                , accessor = .fuelPriceInput
                }


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
